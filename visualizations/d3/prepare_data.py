"""Utility script to convert repository CSV outputs into compact JSON files
for the interactive D3 visualizations.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Dict, List, Set

import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]
AUTHOR_LANG_CSV = (
    REPO_ROOT
    / "Network Analysis"
    / "data"
    / "alldnb_author_language_count_ch2.1_v2.csv"
)
LANG_LANG_CSV = (
    REPO_ROOT
    / "Network Analysis"
    / "results"
    / "alldnb_lang_lang_author_edges_ch2.2_v2.csv"
)
OUTPUT_DIR = Path(__file__).resolve().parent / "data"


def _clean_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Drop unnamed columns and ensure numeric types."""
    unnamed = [c for c in df.columns if c.startswith("Unnamed") or c == ""]
    df = df.drop(columns=unnamed, errors="ignore")
    return df


def _select_top_columns(df: pd.DataFrame, cols: List[str], top_n: int | None) -> Set[str]:
    if not top_n or top_n >= len(cols):
        return set(cols)
    totals = df[cols].sum().sort_values(ascending=False)
    return set(totals.head(top_n).index.tolist())


def _build_author_language_graph(
    min_edge_weight: int,
    top_languages: int | None,
    top_authors: int | None,
) -> Dict:
    df = pd.read_csv(AUTHOR_LANG_CSV)
    df = _clean_columns(df)
    df = df.fillna(0)

    value_columns = [c for c in df.columns if c not in {"author", "na_count"}]
    language_keep = _select_top_columns(df, value_columns, top_languages)

    edges = []
    author_totals: Dict[str, int] = {}
    author_link_counts: Dict[str, int] = {}
    language_totals: Dict[str, int] = {}
    language_link_counts: Dict[str, int] = {}

    for _, row in df.iterrows():
        author = row["author"]
        total_weight = 0
        link_count = 0
        for lang in language_keep:
            weight = int(row.get(lang, 0))
            if weight >= min_edge_weight:
                edges.append({"source": author, "target": lang, "weight": weight})
                total_weight += weight
                link_count += 1
                language_totals[lang] = language_totals.get(lang, 0) + weight
                language_link_counts[lang] = language_link_counts.get(lang, 0) + 1
        if total_weight:
            author_totals[author] = total_weight
            author_link_counts[author] = link_count

    if top_authors:
        ranked_authors = sorted(
            author_totals.items(), key=lambda item: item[1], reverse=True
        )
        keep_authors = {name for name, _ in ranked_authors[:top_authors]}
    else:
        keep_authors = set(author_totals.keys())

    filtered_edges = [edge for edge in edges if edge["source"] in keep_authors]

    # Recompute totals after author filtering to keep metadata honest.
    author_totals = {}
    author_link_counts = {}
    language_totals = {}
    language_link_counts = {}
    for edge in filtered_edges:
        author = edge["source"]
        lang = edge["target"]
        weight = edge["weight"]
        author_totals[author] = author_totals.get(author, 0) + weight
        author_link_counts[author] = author_link_counts.get(author, 0) + 1
        language_totals[lang] = language_totals.get(lang, 0) + weight
        language_link_counts[lang] = language_link_counts.get(lang, 0) + 1

    nodes = []
    for author, total in author_totals.items():
        nodes.append(
            {
                "id": author,
                "label": author,
                "type": "author",
                "totalWeight": total,
                "linkCount": author_link_counts.get(author, 0),
            }
        )
    for lang, total in language_totals.items():
        nodes.append(
            {
                "id": lang,
                "label": lang,
                "type": "language",
                "totalWeight": total,
                "linkCount": language_link_counts.get(lang, 0),
            }
        )

    return {
        "meta": {
            "minEdgeWeight": min_edge_weight,
            "topLanguages": top_languages,
            "topAuthors": top_authors,
            "edgeCount": len(filtered_edges),
            "nodeCount": len(nodes),
        },
        "nodes": nodes,
        "links": filtered_edges,
    }


def _build_language_language_graph(min_edge_weight: int) -> Dict:
    df = pd.read_csv(LANG_LANG_CSV)
    df = _clean_columns(df)
    df = df.fillna(0)

    edges = df[df["weight"] >= min_edge_weight].copy()
    edges = edges.rename(columns={"from": "source", "to": "target"})

    nodes: Dict[str, Dict[str, int]] = {}
    for _, row in edges.iterrows():
        for col in ("source", "target"):
            lang = row[col]
            nodes.setdefault(lang, {"totalWeight": 0, "linkCount": 0})
            nodes[lang]["totalWeight"] += int(row["weight"])
            nodes[lang]["linkCount"] += 1

    node_list = [
        {
            "id": lang,
            "label": lang,
            "type": "language",
            "totalWeight": stats["totalWeight"],
            "linkCount": stats["linkCount"],
        }
        for lang, stats in sorted(nodes.items())
    ]

    edge_list = [
        {"source": row["source"], "target": row["target"], "weight": int(row["weight"])}
        for _, row in edges.iterrows()
    ]

    return {
        "meta": {
            "minEdgeWeight": min_edge_weight,
            "edgeCount": len(edge_list),
            "nodeCount": len(node_list),
        },
        "nodes": node_list,
        "links": edge_list,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Prepare JSON for D3 visualizations.")
    parser.add_argument("--min-author-language-weight", type=int, default=5)
    parser.add_argument("--top-languages", type=int, default=40)
    parser.add_argument("--top-authors", type=int, default=200)
    parser.add_argument("--min-language-link-weight", type=int, default=50)
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=OUTPUT_DIR,
        help="Directory to place JSON outputs.",
    )

    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    author_lang_graph = _build_author_language_graph(
        min_edge_weight=args.min_author_language_weight,
        top_languages=args.top_languages,
        top_authors=args.top_authors,
    )
    language_graph = _build_language_language_graph(
        min_edge_weight=args.min_language_link_weight
    )

    author_output = args.output_dir / "author_language_graph.json"
    lang_output = args.output_dir / "language_language_graph.json"

    author_output.write_text(json.dumps(author_lang_graph, indent=2))
    lang_output.write_text(json.dumps(language_graph, indent=2))

    print(f"Wrote {author_output}")
    print(f"Wrote {lang_output}")


if __name__ == "__main__":
    main()
