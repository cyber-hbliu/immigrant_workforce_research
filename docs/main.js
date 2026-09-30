/* Immigrant wages in metropolitan Philadelphia. Scrollytelling, D3 v7 + scrollama.
   Data (web/data/): trend.csv, origins.csv, land-110m.json, results.json */

const C = {
  ink: "#0d0d0d", ink2: "#595959", muted: "#7f7f7f", grid: "#ececec", axis: "#bababa",
  teal: "#00b0be", tealDark: "#0d7d87", tealPale: "#dff4f4", tealLight: "#8fd7d7",
  orange: "#ea801c", orangeLight: "#f0b077", gold: "#c99b38", grey: "#a1a1a1", grey2: "#d4d4d4", brown: "#eddca5",
  region: { "Asia": "#00b0be", "Latin America": "#ea801c", "Europe": "#c99b38", "Africa": "#a1a1a1" },
  english: ["#8fd7d7", "#00b0be", "#0d7d87", "#0a4f55"]
};
const PHL = [-75.1652, 39.9526];
const fmt$ = d3.format("$,.0f"), fmtPct = d => d3.format(".0f")(d) + "%", fmtN = d3.format(",");
const ease = d3.easeCubicInOut, T = 900;
const state = { data: {}, world: null, charts: {} };

// ---------- helpers ----------
function svgBox(id) {
  const svg = d3.select(id); svg.selectAll("*").remove();
  const { width, height } = svg.node().getBoundingClientRect();
  svg.attr("viewBox", `0 0 ${width} ${height}`);
  return { svg, w: width, h: height };
}
function caption() {}
const tip = d3.select("body").append("div").attr("class", "tip").style("opacity", 0);
function hover(sel, html) {
  sel.style("cursor", "default")
    .on("mousemove", (ev, d) => { tip.style("opacity", 1).html(html(d)).style("left", (ev.pageX + 14) + "px").style("top", (ev.pageY - 10) + "px"); })
    .on("mouseleave", () => tip.style("opacity", 0));
}
function drawPath(sel, dur = 1200, delay = 0) {
  sel.each(function () {
    const L = this.getTotalLength();
    d3.select(this).attr("stroke-dasharray", `${L} ${L}`).attr("stroke-dashoffset", L)
      .transition("draw").delay(delay).duration(dur).ease(ease).attr("stroke-dashoffset", 0);
  });
}
function axes(g, x, y, w, h, opts = {}) {
  g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(opts.yt || 5).tickSize(-w).tickFormat(""));
  g.append("g").attr("class", "axis").attr("transform", `translate(0,${h})`).call(d3.axisBottom(x).ticks(opts.xt || 6).tickFormat(opts.xf || null).tickSizeOuter(0));
  g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(opts.yt || 5).tickFormat(opts.yf || null).tickSizeOuter(0)).select(".domain").remove();
}
function title(g, text) { return g.append("text").attr("class", "title").attr("x", 0).attr("y", -14).text(text); }

// ---------- chapter 2: wage gradient -> decomposition ----------
// ---------- chapter 4b: county roses ----------
function initCounty() {
  const { svg, w, h } = svgBox("#s-county");
  if (!state.data.counties) { state.charts.county = { step() {} }; return; }
  const chart = CH.countyRose(svg, state.data.counties, { width: w, height: h });
  state.charts.county = {
    step(i) {
      chart.select(["Philadelphia", "Salem", "Chester"][i] || "Philadelphia");
      caption("#c-county", "Five percentages for the eleven counties of the metropolitan area, 2022–2024, on one 0 to 100 scale. The wage petal is the foreign-born median hourly wage as a percentage of the native-born median. Descriptive statistics for the same sample.");
    }
  };
}

// ---------- shared: dot-and-interval chart ----------
function intervalChart(g, rows, cw, ch, opts) {
  const y = d3.scaleBand().domain(rows.map(d => d.g)).range([0, ch]).padding(0.5);
  const x = d3.scaleLinear().domain(opts.domain).range([0, cw]);
  g.append("g").attr("class", "grid").call(d3.axisLeft(y).tickSize(-cw).tickFormat(""));
  g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).ticks(6).tickFormat(opts.xf).tickSizeOuter(0));
  g.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 0).attr("y2", ch).attr("stroke", C.ink).attr("stroke-width", 1);
  const r = g.selectAll(".iv").data(rows).join("g").attr("class", "iv").attr("transform", d => `translate(0,${y(d.g) + y.bandwidth() / 2})`);
  r.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.g);
  r.append("line").attr("x1", d => x(d.v - 1.96 * d.se)).attr("x2", d => x(d.v + 1.96 * d.se)).attr("stroke", d => d.c).attr("stroke-width", 3).attr("stroke-linecap", "round");
  r.append("circle").attr("cx", d => x(d.v)).attr("r", 6).attr("fill", d => d.c).attr("stroke", "#fff").attr("stroke-width", 1.5);
  r.append("text").attr("class", "label").attr("x", d => x(d.v + 1.96 * d.se) + 10).attr("dy", 4).text(d => opts.lf(d));
  hover(r, d => `<b>${d.g}</b><br>${(d.v * 100).toFixed(1)}% per SD, SE ${(d.se * 100).toFixed(1)}<br>95% interval ${((d.v - 1.96 * d.se) * 100).toFixed(1)}% to ${((d.v + 1.96 * d.se) * 100).toFixed(1)}%`);
  return r;
}

// ---------- chapter 6: the density gradient ----------
function initSlopes() {
  const showScatter = on => { d3.select("#s-scatter").classed("off", !on); d3.select("#g-slopes").classed("off", on); };
  showScatter(true);
  const sb = svgBox("#s-scatter");
  const Rz = state.data.results;
  const ws = state.pumas ? CH.wageScatter(sb.svg, { width: sb.w, height: sb.h, pumas: state.pumas, t1: Rz.t1, oaxaca: Rz.oaxaca, engColors: C.english }) : { step() {} };
  // the hidden svg shares the visible one's box
  const svg = d3.select("#g-slopes"), w = sb.w, h = sb.h; svg.selectAll("*").remove(); svg.attr("viewBox", `0 0 ${w} ${h}`);
  const chart = CH.gradient(svg, { width: w, height: h, S: Rz.slopes, limited: Rz.enclave.A.limited, inter: Rz.slopes.interaction, sd: 5.4, mean: 12 });
  state.charts.slopes = { step(i) { showScatter(i <= 3); ws.step(i); chart.step(i - 3); } };
}

// ---------- chapter 9: own-language density ----------
function initOwnlang() {
  const { svg, w, h } = svgBox("#g-ownlang");
  const O = JSON.parse(JSON.stringify(state.data.results.ownlang));
  // within-occupation interactions, Table 3 (0.021, SE 0.013 Philadelphia; 0.028, SE 0.007 New York)
  O.Philadelphia.inter_occ = 0.021; O.Philadelphia.inter_occ_se = 0.013; O["New York"].inter_occ = 0.028; O["New York"].inter_occ_se = 0.007;
  // the five highest own-language (Spanish) values, section 4.5
  const top = [{ id: "PA03225", label: "North Philadelphia", value: 41 }, { id: "NJ02101", label: "Camden and Gloucester City, NJ", value: 38 }, { id: "PA03222", label: "Central and Lower Northeast", value: 22 }, { id: "NJ02501", label: "Salem and Cumberland (Bridgeton), NJ", value: 18 }, { id: "PA03223", label: "North Delaware", value: 17 }];
  // direct check of the selection account, section 5.2
  const sel = [{ g: "leavers vs stayers", v: -6, se: 18 }, { g: "same, within occupation", v: -13.3, se: 11.6 }];
  const chart = CH.ownLang(svg, { width: w, height: h, pumas: state.pumas, O, top, sel, lang: state.data.languagePuma });
  const ls = d3.select("#lang-sel");
  if (chart.langs.length) { ls.style("display", null).selectAll("option").data(["paper"].concat(chart.langs)).join("option").attr("value", d => d).text(d => d === "paper" ? "the five values in the paper" : d); ls.on("change", () => chart.setLang(ls.node().value)); }
  else ls.style("display", "none");
  state.charts.ownlang = { step: i => chart.step(i) };
}

// ---------- chapter 9: pseudo-panel and duration-matched ----------
function initPanel() {
  const { svg, w, h } = svgBox("#g-panel");
  const tab7 = (state.data.tables || []).find(t => t.label === "tab7");
  if (!tab7) return;
  const chart = CH.cohortPanel(svg, { width: w, height: h, cells: state.data.results.cells, tab7: tab7.rows, fe: { b: 9.7, se: 10.7 }, regionColor: C.region });
  state.charts.panel = { step: i => chart.step(i) };
}

// ---------- chapters whose graphic is the interactive panel from explore.js ----------
function setCtl(id, value, ev = "change") { const el = document.getElementById(id); if (!el) return; if (el.type === "checkbox") el.checked = !!value; else el.value = value; el.dispatchEvent(new Event(ev)); }
function initInteractive() {
  state.charts.globe = { step(i) { caption("#c-globe", i === 0 ? "Residents of the metropolitan area born outside the United States by country of birth, 2022–2024, all ages. Line width and color are the count. Hover a country, drag to turn the globe." : i === 1 ? "Historical and combined ACS codes are mapped to a present-day country. Codes without a boundary are counted in the total but not drawn." : "The foreign-born count is for the city of Philadelphia, from one-year ACS estimates."); } };
  state.charts.cohort = { step(i) { setCtl("co-t", i === 0 ? 10 : 20, "input"); caption("#c-cohort", "Predicted wage gap to comparable natives by years since migration, from the cohort specification with English, occupation, PUMA and year fixed effects. Move the slider or switch cohorts off."); } };
  state.charts.map = { step(i) { setCtl("area-var", "puma_fb_prop"); setCtl("area-flows", i === 0 ? "none" : "city_to_suburb"); caption("#c-map", i === 0 ? "Foreign-born percentage of residents by PUMA, with the same 48 PUMAs as dots against foreign-born median wage. Hover either side, or drag a box across the scatter." : "Where foreign-born residents who left the city for a suburb in the past year arrived, 2022–2024. Circle size is the annual count."); } };
  state.charts.enclave = { step(i) { if (window.occPanelsChart) window.occPanelsChart.step(i); caption("#c-enclave", i === 0 ? "Fitted log wage by area immigrant density from the two-level model, no occupation controls. Move the slider to read both groups at any density." : "The same lines with occupation fixed effects. The distance between them is the within-occupation differential."); } };
  state.charts.occ = { step(i) { setCtl("occ-sort", i === 0 ? "Limited English" : "wage"); caption("#c-occ", i === 0 ? "Percentage of each group in twelve occupation groups, 2022–2024, sorted by the limited-English percentage. Color is the group's median hourly wage there. The right column divides the limited-English median by the proficient median." : "Sorted by the native-born median wage. Descriptive statistics for the model sample."); } };
  state.charts.compare = { step(i) { if (window.cityCompareChart) window.cityCompareChart.step(i); setCtl("inter-bonf", i === 2); caption("#c-compare", i === 0 ? "Every limited-English × density interaction in the paper, sorted by |t|, with 95 percent intervals. Hover a point for the estimate, the standard error and the sample." : "Marks with a black rim clear the Bonferroni line for seventeen tests, |t| above 2.92."); } };
  state.charts.moves = { step(i) {
    setCtl("flow-group", i === 4 ? "nb" : "fb"); setCtl("flow-movers", i !== 3); setCtl("mv-var", "1");
    const L = window.flowLink || {};
    if (L.focusWindow) L.focusWindow(i === 1 ? "2022-2024" : null);
    if (L.highlightAll) L.highlightAll(i === 1 ? "city_to_suburb" : null);
    d3.select("#s-moves").classed("dim", i === 0 || i === 1);
  } };
  state.charts.grid = { step(i) { setCtl("cell-metric", i === 0 ? "median_hourly" : i === 1 ? "limited_eng_prop" : "suburb_prop"); } };
}

// ---------- boot ----------
async function boot() {
  const [trend, origins, world, results] = await Promise.all([
    d3.csv("data/trend.csv"), d3.csv("data/origins.csv"), d3.json("data/land-110m.json"), d3.json("data/results.json")
  ]);
  state.data = { trend, origins, results };
  try { state.data.tables = await d3.json("data/tables.json"); } catch (e) { state.data.tables = []; }
  state.world = topojson.feature(world, world.objects.land);
  try { state.pumas = await d3.json("data/pumas.geojson"); } catch (e) { state.pumas = null; }
  try { state.data.mobility = await d3.json("data/mobility.json"); } catch (e) { state.data.mobility = null; }
  try { state.data.occupations = await d3.csv("data/occupations.csv"); } catch (e) { state.data.occupations = null; }
  try { state.data.counties = await d3.csv("data/county_profiles.csv"); } catch (e) { state.data.counties = null; }
  try { const mr = await d3.csv("data/move_rates.csv"); state.data.moveRates = mr.map(d => ({ ...d, fb: d.fb === "TRUE" || d.fb === "true", n: +d.n, pct: +d.pct })); }
  catch (e) { state.data.moveRates = state.data.mobility ? state.data.mobility.move_rates.map(d => ({ ...d, fb: true })) : []; }
  try { state.data.languagePuma = await d3.csv("data/language_puma.csv"); } catch (e) { state.data.languagePuma = null; }
  try { const ir = await d3.csv("data/interactions_region.csv"); state.data.interactionsRegion = ir.filter(d => d.Estimate).map(d => ({ region: d.sample, est: d.Estimate, se: d["Std. Error"] })); } catch (e) { state.data.interactionsRegion = []; }
  const inits = { county: initCounty, slopes: initSlopes, ownlang: initOwnlang, panel: initPanel, interactive: initInteractive };
  function initAll() { Object.values(inits).forEach(f => f()); }
  initAll();
  const scroller = scrollama();
  scroller.setup({ step: ".step", offset: 0.55 }).onStepEnter(({ element }) => {
    d3.selectAll(".step").classed("is-active", false); d3.select(element).classed("is-active", true);
    const [ch, i] = element.dataset.step.split("-");
    if (state.charts[ch]) state.charts[ch].step(+i);
  });
  let rt; window.addEventListener("resize", () => { clearTimeout(rt); rt = setTimeout(() => { initAll(); scroller.resize(); }, 200); });
}
boot();
