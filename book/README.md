# Numbat Documentation

This directory contains the Numbat documentation, built with [Zensical](https://zensical.org/).

## Prerequisites

- [uv](https://docs.astral.sh/uv/) - Python package manager
- Rust toolchain (for generating function/unit reference docs)

## Development

Install dependencies and start a local preview server:

```bash
uv run zensical serve
```

This will start a development server at `http://localhost:8000` with live reload.

## Building

To build the documentation (including auto-generated content):

```bash
uv run python build_docs.py
```

This will:
1. Generate example files from `examples/*.nbt`
2. Generate function reference documentation
3. Generate the units list
4. Build the static site to `site/`

To build without regenerating auto-generated content:

```bash
uv run zensical build
```

## Deployment

To build and deploy to the production server:

```bash
./deploy.sh
```

## Structure

- `src/` - Markdown source files
- `zensical.toml` - Zensical configuration
- `build_docs.py` - Build script that generates auto-generated content
- `site/` - Generated static site (gitignored)
