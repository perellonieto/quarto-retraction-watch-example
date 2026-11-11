# AGENTS.md

## Project overview
This workspace is a Quarto website. Page content lives in `.qmd` files, and site configuration lives in `_quarto.yml`.

## Editing conventions
- Keep page content in `.qmd` files and site structure/theme settings in `_quarto.yml`.
- Preserve YAML front matter at the top of each page when editing content.
- The project uses Quarto with Python/Jupyter support, so Python code blocks should remain valid and self-contained.

## Preview and build workflow
- For local development, preview the site with:
  - `quarto preview`
- If the local virtual environment is needed, activate it first:
  - `source venv/bin/activate`
- If you want to rebuild the site without the live preview server, use:
  - `quarto render`
- Generated output is typically written to `_site/`.

## When debugging changes
- If a page does not render correctly, check the page’s YAML front matter and any embedded code cells first.
- Keep navbar links consistent with the links defined in `_quarto.yml`.
- Prefer previewing the site after content changes to verify the rendered result quickly.
