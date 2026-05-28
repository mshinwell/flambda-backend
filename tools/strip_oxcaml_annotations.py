#!/usr/bin/env python3
"""Strip OxCaml-specific annotations from stdlib-like-upstream files.

The goal is to make these files use only upstream OCaml syntax, by removing:
- Mode annotations on values/types: `@@ portable`, `@ contended`, etc.
- Kind annotations on type params: `('a : value_or_null mod separable)`, etc.
- Kind annotations on type declarations: `type t : kind = ...`, etc.
- OxCaml-specific attributes: `[@local_opt]`, `[@layout_poly]`,
  `[@@unsafe_allow_any_mode_crossing ...]`, `[@@alert unsafe_multidomain ...]`.

Attributes NOT stripped (recognized by both upstream OCaml's Flambda and
OxCaml's Flambda 2):
- `[@@@ocaml.flambda_o3]`, `[@@@ocaml.flambda_oclassic]`

Heuristics, edge cases handled:
- Universal quantifier prefixes with kind annotations
  (`('a : k1) ('b : k2). T` -> `T`) are removed entirely, not converted to
  `'a 'b. T`, since upstream OCaml stdlib doesn't use such explicit polymorphism.
- Record field explicit polymorphism (e.g. `effc: 'c.'c t`) is preserved.
- `[@local_opt]` inside parens like `(int[@local_opt])` becomes plain `int`,
  with the now-redundant parens stripped.
- `sig @@ portable include ...` becomes `sig include ...` while preserving
  inter-token spacing.
- `type ('a : k, !'b : k) t` -> `type ('a, !'b) t` (with variance markers).
- Comma-separated kinds inside parens are handled.
- The leading `# 1 "file.ml"` directive (line/file directives in stdlib) is
  preserved.

Usage:
  python3 strip_oxcaml.py <directory> [--dry-run]

Run on stdlib-like-upstream/ to strip annotations. Files are modified in place
unless --dry-run is passed.
"""
import re
import os
import sys


# ---------------------------------------------------------------------------
# Mode and kind keyword inventory.
# ---------------------------------------------------------------------------

# Mode keywords that may appear after `@` or `@@`. These describe the modal
# constraints on a value or type:
MODE_AXES = [
    # Portability axis
    'portable', 'nonportable', 'stateless',
    # Uniqueness axis
    'aliased', 'unique',
    # Contention axis
    'contended', 'shared', 'uncontended',
    # Linearity axis
    'many', 'once',
    # Locality axis
    'global', 'local',
    # Yielding axis
    'unyielding', 'yielding',
    # Visibility / mutability axes
    'immutable', 'mutable_data',
    'read', 'read_write',
    # Externality axis
    'external_', 'nonexternal',
]
MODE_AXES_RE = '|'.join(MODE_AXES)
# A mode list is one or more mode keywords separated by whitespace, with no
# other punctuation interleaved.
MODE_LIST_RE = rf'(?:{MODE_AXES_RE})(?:\s+(?:{MODE_AXES_RE}))*'

# Layout/kind keywords used in kind annotations and `mod`/`with` clauses.
KIND_BASES = [
    'value_or_null', 'value', 'any',
    'bits8', 'bits16', 'bits32', 'bits64',
    'word',
    'float64', 'float32',
    'immediate', 'immediate64',
    'mutable_data', 'immutable_data', 'sync_data',
]
KIND_BASES_RE = '|'.join(KIND_BASES)
# Modifier keywords usable after `mod`. These include all the mode axes plus
# layout-only ones like `separable` and `non_float`.
KIND_MOD_KEYWORDS = MODE_AXES + ['separable', 'non_float', 'float']
KIND_MOD_RE = '|'.join(KIND_MOD_KEYWORDS)

# Match the `with` tail of a kind expression. The contents can include type
# expressions (e.g. `with 'a`, `with type T`), and multiple `with` clauses
# can chain (e.g. `with 'a with 'b`). We accept anything up to a likely
# delimiter (`)`, `=`, `,`, `;`, newline) and allow chained `with` clauses.
WITH_TAIL_RE = r'(?:\s+with\s+(?:[^\n)=,;]|\([^)]*\))+)*'

# A kind expression: BASE [mod MOD ...] [with ...]
KIND_RE = (
    rf'(?:{KIND_BASES_RE})'
    rf'(?:\s+mod\s+(?:{KIND_MOD_RE})(?:\s+(?:{KIND_MOD_RE}))*)?'
    + WITH_TAIL_RE
)


# ---------------------------------------------------------------------------
# Attribute removal
# ---------------------------------------------------------------------------

def strip_local_opt(content):
    """Remove `[@local_opt]` attributes.

    Common forms in OxCaml stdlib (always parenthesized):
      `('a array[@local_opt])` -> `'a array`
      `(int[@local_opt])` -> `int`
      `('a[@local_opt])` -> `'a`
      `(('a, 'b, 'c) t[@local_opt])` -> `('a, 'b, 'c) t`

    When the attribute is wrapped in parens with balanced contents, the outer
    parens are stripped too. Balanced paren-matching is required because the
    inner type may itself contain parens (e.g. tuple type args).
    """
    result = []
    pos = 0
    attr = '[@local_opt]'
    while pos < len(content):
        idx = content.find(attr, pos)
        if idx < 0:
            result.append(content[pos:])
            break
        attr_end = idx + len(attr)
        # If `[@local_opt]` is immediately followed by `)`, look backwards from
        # `idx` for the matching `(`.
        if attr_end < len(content) and content[attr_end] == ')':
            close_paren = attr_end
            depth = 1
            j = idx - 1
            open_paren = -1
            while j >= pos:
                if content[j] == ')':
                    depth += 1
                elif content[j] == '(':
                    depth -= 1
                    if depth == 0:
                        open_paren = j
                        break
                j -= 1
            if open_paren >= 0:
                # Emit up to `(`, then inner content, then skip `[@local_opt])`.
                result.append(content[pos:open_paren])
                result.append(content[open_paren + 1:idx].rstrip())
                pos = close_paren + 1
                continue
        # Pattern not matched; just strip the bare attribute.
        result.append(content[pos:idx])
        # Eat preceding whitespace on the same line if any.
        while result and result[-1].endswith((' ', '\t')):
            result[-1] = result[-1].rstrip(' \t')
            break
        pos = attr_end
    return ''.join(result)


def strip_layout_poly(content):
    """Remove `[@layout_poly]` attribute on `external` declarations."""
    return re.sub(r'\s*\[@layout_poly\]', '', content)


def strip_unsafe_allow_mode_crossing(content):
    """Remove `[@@unsafe_allow_any_mode_crossing ...]` attribute."""
    return re.sub(
        r'\s*\[@@unsafe_allow_any_mode_crossing(?:\s+"[^"]*")?\]',
        '',
        content,
    )


def strip_unsafe_multidomain_alert(content):
    """Remove `[@@alert unsafe_multidomain "..."]` attribute.

    These typically appear on their own line below a `val` declaration.
    """
    # Eat the preceding newline and indentation so we don't leave a blank line.
    content = re.sub(
        r'\n[ \t]*\[@@alert unsafe_multidomain\s+"[^"]*"\]',
        '',
        content,
    )
    # Also handle inline occurrences.
    content = re.sub(
        r'\s*\[@@alert unsafe_multidomain\s+"[^"]*"\]',
        '',
        content,
    )
    return content


# ---------------------------------------------------------------------------
# Kind annotation removal
# ---------------------------------------------------------------------------

def strip_quantifier_prefix(content):
    """Remove kind-annotated universal quantifier prefixes.

    Match patterns like:
      `('a : value_or_null mod separable).` -> ``
      `('a : value_or_null) ('b : value_or_null).` -> ``
      `('a : k1) ('b : k2) ('c : k3).` -> ``

    The whole quantifier prefix (params + trailing dot) is removed because
    upstream OCaml stdlib does not use explicit universal quantification on
    `val`/`external` declarations.
    """
    # Single quantifier element: `('var : KIND)` or `(!'var : KIND)`
    quant_elem = rf"\(\s*!?'(?:\w+)\s*:\s*{KIND_RE}\s*\)"
    # A quantifier prefix is one or more elements followed by `.`
    # At least one element must have a kind constraint (we know they all do here).
    # Also consume any trailing whitespace (including the space after `.`) so
    # we don't leave a double-space artifact.
    pattern = rf"(?:{quant_elem})(?:\s+{quant_elem})*\s*\.\s*"
    return re.sub(pattern, '', content)


def strip_kind_on_typeparam(content):
    """Remove kind annotations on type parameters.

    Forms:
      `('a : value_or_null mod separable)` -> `'a`
      `('a : any)` -> `'a`
      `(!'a : value_or_null)` -> `!'a`
      `(_ : value_or_null)` -> `_`

    This should be run AFTER `strip_quantifier_prefix`, so the only remaining
    `('a : KIND)` patterns are inside type declarations like
    `type ('a : kind) t = ...`.
    """
    # `('var : KIND)` -> `'var`; preserve variance marker.
    pattern = rf"\(\s*(!?)'(\w+)\s*:\s*{KIND_RE}\s*\)"
    content = re.sub(pattern, r"\1'\2", content)
    # `(_ : KIND)` -> `_`
    pattern_underscore = rf"\(\s*(_)\s*:\s*{KIND_RE}\s*\)"
    content = re.sub(pattern_underscore, r"\1", content)
    return content


def strip_kind_in_comma_list(content):
    """Strip kind annotations within comma-separated parameter lists.

    `type ('a : k, 'b : k, !'c : k) t = ...`
    -> `type ('a, 'b, !'c) t = ...`

    `(_ : k1, _ : k2) eq` -> `(_, _) eq`

    These are inside the type-decl parameter list; they don't get matched by
    `strip_kind_on_typeparam` because they're not wrapped in their own parens.
    Within `type (X) t`, each `X` is comma-separated.

    NOTE: only handles the case where there is more than one comma-separated
    element. The single-element case `('a : k)` is left to
    `strip_kind_on_typeparam`, which produces `'a` (without parens) so we match
    upstream OCaml style.
    """
    def fix_paren_list(m):
        body = m.group(1)
        parts = []
        depth = 0
        current = []
        for ch in body:
            if ch == '(' :
                depth += 1
                current.append(ch)
            elif ch == ')':
                depth -= 1
                current.append(ch)
            elif ch == ',' and depth == 0:
                parts.append(''.join(current))
                current = []
            else:
                current.append(ch)
        parts.append(''.join(current))

        new_parts = []
        for p in parts:
            stripped = re.sub(
                rf"^\s*(!?'\w+|_)\s*:\s*{KIND_RE}\s*$",
                r"\1",
                p.strip(),
            )
            new_parts.append(stripped)
        return '(' + ', '.join(new_parts) + ')'

    # Require at least one comma so single-param `('a : k)` is left for
    # `strip_kind_on_typeparam` to strip cleanly (without leaving redundant
    # parens).
    pattern = re.compile(
        rf"\((\s*(?:!?'\w+|_)\s*:\s*{KIND_RE}"
        rf"(?:\s*,\s*(?:!?'\w+|_)\s*:\s*{KIND_RE})+\s*)\)"
    )
    return pattern.sub(lambda m: fix_paren_list(m), content)


def strip_kind_on_type_decl(content):
    """Strip kind annotations on type declarations.

    Forms handled (where KIND is a kind expression possibly with `mod` and `with`):
      `type NAME : KIND = ...` -> `type NAME = ...`
      `type ('a) NAME : KIND = ...` -> `type ('a) NAME = ...`
      `type NAME : KIND` (abstract) -> `type NAME`

    Where NAME may be preceded by type parameters (`'a`, `('a, 'b)`).
    """
    # Build a kind-suffix pattern: `: KIND` at the named position.
    kind_suffix = rf"\s*:\s*{KIND_RE}"

    # We want to find `type ... NAME` and any trailing `: KIND` before `=`,
    # newline, or end-of-statement.

    # Pattern: capture the type-decl preamble up to NAME, then `: KIND`, then
    # rest. We have to be careful not to match `type ('a : k) ...` (handled
    # elsewhere) or `(type a : k)` (locally abstract).

    # Easiest: match a line beginning with `type` and find the `: KIND` before
    # `=` or end-of-line.
    lines = content.split('\n')
    new_lines = []
    for line in lines:
        # If line contains a type declaration with a kind annotation, fix it.
        # We use re.match to anchor at the start; the whole prefix + kind
        # tail + everything-after is captured so we can re-emit the trailing
        # `=...` or end-of-line correctly.
        m = re.match(
            rf'^(\s*(?:and|type)\s+(?:\([^)]*\)\s+|!?\'?\w+\s+)*\w+)'
            rf'{kind_suffix}'
            rf'(\s*(?:=.*)?)$',
            line,
        )
        if m:
            prefix = m.group(1)
            tail = m.group(2)
            # If the kind suffix greedily consumed the whitespace before `=`,
            # re-introduce a separating space.
            if tail.startswith('=') and prefix and not prefix.endswith(' '):
                tail = ' ' + tail
            new_lines.append(prefix + tail)
        else:
            new_lines.append(line)
    return '\n'.join(new_lines)


def strip_kind_on_locally_abstract(content):
    """Remove kind annotations in `(type ...)` locally abstract types.

    Handles both single-binding and multi-binding forms:
      `(type a : kind)` -> `(type a)`
      `(type a b c)` (no kinds, no change)
      `(type (a : k) (b : k) (c : k))` -> `(type a b c)`
      `(type (a : k))` -> `(type a)`
    """
    # Form A: bare `(type NAMES : KIND)` - colon directly after name list.
    pattern = rf"\(\s*type\s+((?:\w+\s+)*\w+)\s*:\s*{KIND_RE}\s*\)"
    content = re.sub(pattern, r"(type \1)", content)

    # Form B: parenthesized per-binding `(type (a : k) (b : k) ...)`.
    # We rewrite the inside of `(type ...)` if any binding has the form
    # `(name : kind)`.
    def repl_form_b(m):
        inner = m.group(1)
        # Split inner into bindings, which are either `(name : kind)` or `name`.
        bindings = re.findall(
            rf"\(\s*(\w+)\s*:\s*{KIND_RE}\s*\)|(\w+)", inner)
        names = [a or b for (a, b) in bindings]
        if not names:
            return m.group(0)
        return f"(type {' '.join(names)})"

    # Match `(type ...)` where the body contains at least one `(name : kind)`.
    content = re.sub(
        rf"\(\s*type\s+((?:\s*(?:\(\s*\w+\s*:\s*{KIND_RE}\s*\)|\w+))+)\s*\)",
        repl_form_b,
        content,
    )
    return content


# ---------------------------------------------------------------------------
# Mode annotation removal
# ---------------------------------------------------------------------------

def strip_at_at_modes(content):
    """Remove `@@ mode1 mode2 ...` mode annotations.

    `@@` is also OCaml's reverse-application operator, so we only match when
    `@@` is followed by a sequence of mode keywords.
    """
    pattern = rf'\s+@@\s+(?:{MODE_LIST_RE})(?![a-zA-Z0-9_])'
    return re.sub(pattern, '', content)


def strip_at_modes(content):
    """Remove ` @ mode1 mode2 ...` mode annotations.

    `@` is OCaml's list concatenation operator; we only match when `@` is
    followed by a sequence of mode keywords.
    """
    pattern = rf'\s+@\s+(?:{MODE_LIST_RE})(?![a-zA-Z0-9_])'
    return re.sub(pattern, '', content)


def strip_top_level_modes(content):
    """Remove `@@ MODES` lines that appear by themselves at file top-level.

    These are module-level mode annotations, e.g. `@@ portable` on its own line
    near the top of a `.mli`. We restrict the whitespace match to spaces and
    tabs (not newlines) so we don't accidentally swallow blank lines around
    the annotation.
    """
    return re.sub(
        rf'^[ \t]*@@[ \t]+{MODE_LIST_RE}[ \t]*\r?\n',
        '',
        content,
        flags=re.MULTILINE,
    )


# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

def cleanup_whitespace(content):
    """Tidy up whitespace artifacts left by other passes."""
    # Strip trailing whitespace from each line.
    content = re.sub(r'[ \t]+$', '', content, flags=re.MULTILINE)
    # Collapse 4+ consecutive newlines into 3 (i.e. at most two blank lines).
    # This preserves the original document's intentional blank-line spacing
    # while still tidying up the larger gaps that can result when our passes
    # remove a line entirely.
    content = re.sub(r'\n{4,}', '\n\n\n', content)
    return content


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

PIPELINE = [
    # Attributes first so we don't have to consider them in other patterns.
    strip_local_opt,
    strip_layout_poly,
    strip_unsafe_allow_mode_crossing,
    strip_unsafe_multidomain_alert,
    # Kind annotations: strip universal quantifiers first (the whole prefix),
    # then any remaining individual kind annotations.
    strip_quantifier_prefix,
    strip_kind_in_comma_list,
    strip_kind_on_typeparam,
    strip_kind_on_locally_abstract,
    strip_kind_on_type_decl,
    # Mode annotations.
    strip_top_level_modes,
    strip_at_at_modes,
    strip_at_modes,
    # Final cleanup.
    cleanup_whitespace,
]


def process_file(path):
    with open(path, 'r') as f:
        original = f.read()
    content = original
    for step in PIPELINE:
        content = step(content)
    if content != original:
        with open(path, 'w') as f:
            f.write(content)
        return True
    return False


EXTENSIONS = ('.ml', '.mli', '.ml.in', '.mli.in')


def iter_target_files(base):
    """Yield (relpath, abspath) pairs for files to process."""
    for root, dirs, files in os.walk(base):
        for name in sorted(files):
            if not any(name.endswith(ext) for ext in EXTENSIONS):
                continue
            abspath = os.path.join(root, name)
            relpath = os.path.relpath(abspath, base)
            yield relpath, abspath


def main():
    if len(sys.argv) < 2:
        print("Usage: strip_oxcaml.py <dir> [--dry-run]", file=sys.stderr)
        sys.exit(1)
    target_dir = sys.argv[1]
    dry_run = '--dry-run' in sys.argv[2:]

    changed = []
    for relpath, abspath in iter_target_files(target_dir):
        if dry_run:
            with open(abspath) as f:
                original = f.read()
            content = original
            for step in PIPELINE:
                content = step(content)
            if content != original:
                changed.append(relpath)
        else:
            if process_file(abspath):
                changed.append(relpath)

    label = 'Would modify' if dry_run else 'Modified'
    print(f"{label} {len(changed)} files:")
    for name in changed:
        print(f"  {name}")


if __name__ == '__main__':
    main()
