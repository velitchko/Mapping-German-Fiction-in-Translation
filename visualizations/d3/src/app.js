import { renderAuthorLanguageNetwork } from "./authorLanguage.js";
import { renderLanguageLanguageNetwork } from "./languageLanguage.js";
import { downloadJSON } from "./utils.js";

const AUTHOR_DATA_URL = "./data/author_language_graph.json";
const LANGUAGE_DATA_URL = "./data/language_language_graph.json";

let authorData = null;
let languageData = null;
let authorFiltered = null;
let languageFiltered = null;

const authorSlider = document.getElementById("authorMinWeight");
const authorSliderValue = document.getElementById("authorMinWeightValue");
const languageSlider = document.getElementById("languageMinWeight");
const languageSliderValue = document.getElementById("languageMinWeightValue");
const authorDownloadBtn = document.getElementById("downloadAuthorJson");
const languageDownloadBtn = document.getElementById("downloadLanguageJson");

function renderAuthor() {
  if (!authorData) return;
  const minWeight = Number(authorSlider.value);
  authorSliderValue.textContent = minWeight;
  authorFiltered = renderAuthorLanguageNetwork("#authorLanguageChart", authorData, {
    minWeight,
  });
}

function renderLanguage() {
  if (!languageData) return;
  const minWeight = Number(languageSlider.value);
  languageSliderValue.textContent = minWeight;
  languageFiltered = renderLanguageLanguageNetwork("#languageLanguageChart", languageData, {
    minWeight,
  });
}

async function init() {
  try {
    const [authorJson, languageJson] = await Promise.all([
      fetch(AUTHOR_DATA_URL).then((res) => {
        if (!res.ok) throw new Error(`Failed to load ${AUTHOR_DATA_URL}`);
        return res.json();
      }),
      fetch(LANGUAGE_DATA_URL).then((res) => {
        if (!res.ok) throw new Error(`Failed to load ${LANGUAGE_DATA_URL}`);
        return res.json();
      }),
    ]);

    authorData = authorJson;
    languageData = languageJson;
    renderAuthor();
    renderLanguage();
  } catch (error) {
    console.error(error);
    const main = document.querySelector("main");
    const banner = document.createElement("div");
    banner.style.background = "#fee2e2";
    banner.style.color = "#991b1b";
    banner.style.padding = "1rem";
    banner.style.margin = "1rem";
    banner.style.borderRadius = "8px";
    banner.textContent = `Unable to load data. Run prepare_data.py first. (${error.message})`;
    main.prepend(banner);
  }
}

authorSlider.addEventListener("input", renderAuthor);
languageSlider.addEventListener("input", renderLanguage);

authorDownloadBtn.addEventListener("click", () => {
  if (authorFiltered) {
    downloadJSON("author_language_filtered.json", authorFiltered);
  }
});

languageDownloadBtn.addEventListener("click", () => {
  if (languageFiltered) {
    downloadJSON("language_language_filtered.json", languageFiltered);
  }
});

window.addEventListener("resize", () => {
  renderAuthor();
  renderLanguage();
});

init();
