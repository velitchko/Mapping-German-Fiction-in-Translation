# Interactive D3 Visualizations

This folder hosts browser-based explorations of the German fiction translation datasets. The workflow has two phases:

1. Convert the CSV outputs from the R analysis into compact JSON payloads (`prepare_data.py`).
2. Open `index.html` in a local web server to interact with the networks.

## 1. Prepare the data

The script expects the cleaned CSV files already present under `Network Analysis/`.

```powershell
cd c:\Users\velit\Documents\Projects\Mapping-German-Fiction-in-Translation\visualizations\d3
python -m venv .venv
.\.venv\Scripts\Activate.ps1
pip install -r requirements.txt
python prepare_data.py --min-author-language-weight 5 --top-languages 40 --top-authors 200 --min-language-link-weight 50
```

Flags let you trim the graphs if the browser becomes sluggish:

- `--min-author-language-weight`: minimum number of translations to keep an author→language edge.
- `--top-languages`: keep only the most translated destination languages (set to `0` for all).
- `--top-authors`: keep the most translated authors (set to `0` for all).
- `--min-language-link-weight`: minimum weight for language→language edges.

Running the script writes two JSON files under `data/` that the D3 apps will fetch.

## 2. Launch the visualizations

Use any static web server so the browser can load the JSON files via `fetch`. A simple option with Python:

```powershell
cd c:\Users\velit\Documents\Projects\Mapping-German-Fiction-in-Translation\visualizations\d3
python -m http.server 8000
```

Then navigate to http://localhost:8000/ in your browser. The page contains:

- **Author ⇄ Language bipartite network** where edge widths represent translation counts.
- **Language ⇄ Language network** built from shared-author weights.

Each chart exposes lightweight controls to adjust the minimum edge weight on the fly, tooltips with metadata, and download buttons for the currently filtered data.

## Customisation ideas

- Extend `prepare_data.py` with additional filters (e.g., year slices) before exporting JSON.
- Replace the default force-layout parameters inside `src/authorLanguage.js` or `src/languageLanguage.js` to emphasise community structures.
- Swap in a different static server (e.g., `npx serve`) or embed the page inside a Quarto/Reveal presentation.
