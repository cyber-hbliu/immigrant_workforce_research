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
function caption(id, text) { d3.select(id).text(text); }
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

// ---------- chapter 1: globe -> trend ----------
function initGlobe() {
  const { svg, w, h } = svgBox("#g-globe");
  const g = svg.append("g");
  const r = Math.min(w, h) * 0.4;
  const proj = d3.geoOrthographic().scale(r).translate([w / 2, h / 2]).rotate([75, -20]).clipAngle(90);
  const path = d3.geoPath(proj);
  g.append("circle").attr("cx", w / 2).attr("cy", h / 2).attr("r", r).attr("fill", "#f6f5f1").attr("stroke", C.axis);
  const land = g.append("path").datum(state.world).attr("fill", "#e4e2dc").attr("stroke", "#fff").attr("stroke-width", 0.5).attr("d", path);
  const grat = g.append("path").datum(d3.geoGraticule10()).attr("fill", "none").attr("stroke", "#e6e6e6").attr("stroke-width", 0.4).attr("d", path);
  const regionOf = { CHN: "Asia", IND: "Asia", VNM: "Asia", KOR: "Asia", PHL: "Asia", PAK: "Asia", BGD: "Asia",
    DOM: "Latin America", MEX: "Latin America", JAM: "Latin America", HTI: "Latin America", BRA: "Latin America", GTM: "Latin America",
    UKR: "Europe", ITA: "Europe", RUS: "Europe", GBR: "Europe", LBR: "Africa", NGA: "Africa", CMR: "Africa" };
  const arcData = state.data.origins.map(o => ({ type: "LineString", coordinates: [[+o.lon, +o.lat], PHL], pct: +o.pct, name: o.country, region: regionOf[o.iso3] || "Other" }));
  const sw = d3.scaleSqrt().domain([0, d3.max(arcData, d => d.pct)]).range([0.8, 6]);
  const arcs = g.append("g").selectAll("path").data(arcData).join("path")
    .attr("fill", "none").attr("stroke", C.tealDark).attr("stroke-opacity", 0.7).attr("stroke-width", d => sw(d.pct)).attr("d", path);
  const phl = g.append("circle").attr("r", 5).attr("fill", C.orange).attr("stroke", "#fff").attr("stroke-width", 1.5);
  const legend = g.append("g").attr("transform", `translate(${w - 150},${h - 90})`).style("opacity", 0);
  Object.entries(C.region).forEach(([k, col], i) => {
    legend.append("rect").attr("x", 0).attr("y", i * 18).attr("width", 12).attr("height", 3).attr("fill", col);
    legend.append("text").attr("x", 18).attr("y", i * 18 + 4).text(k);
  });
  let rot = 75, timer;
  function render() {
    proj.rotate([rot, -20]); land.attr("d", path); grat.attr("d", path); arcs.attr("d", path);
    const p = proj(PHL); phl.attr("cx", p[0]).attr("cy", p[1]);
  }
  render();
  // trend chart, hidden until step 2
  const m = { t: 40, r: 30, b: 40, l: 60 }, tw = w - m.l - m.r, th = h - m.t - m.b;
  const tg = svg.append("g").attr("transform", `translate(${m.l},${m.t})`).style("opacity", 0);
  const tr = state.data.trend;
  const x = d3.scaleLinear().domain(d3.extent(tr, d => +d.year)).range([0, tw]);
  const y = d3.scaleLinear().domain([170000, 250000]).range([th, 0]);
  axes(tg, x, y, tw, th, { xf: d3.format("d"), yf: d => d3.format(",")(d), yt: 4 });
  title(tg, "Foreign-born residents of Philadelphia, 2013 to 2024");
  const line = d3.line().x(d => x(+d.year)).y(d => y(+d.foreign_born)).curve(d3.curveMonotoneX);
  const tl = tg.append("path").datum(tr).attr("fill", "none").attr("stroke", C.teal).attr("stroke-width", 2.5).attr("d", line);
  tg.selectAll("circle").data(tr).join("circle").attr("cx", d => x(+d.year)).attr("cy", d => y(+d.foreign_born)).attr("r", 3.5).attr("fill", C.teal);
  const last = tr[tr.length - 1], first = tr[0];
  tg.append("text").attr("class", "label").attr("x", x(+last.year) - 6).attr("y", y(+last.foreign_born) - 12).attr("text-anchor", "end").text(`${fmtN(+last.foreign_born)} · ${last.pct_foreign_born}% of residents`);
  tg.append("text").attr("class", "note").attr("x", x(+first.year) + 6).attr("y", y(+first.foreign_born) + 18).text(`${fmtN(+first.foreign_born)} · ${first.pct_foreign_born}%`);
  state.charts.globe = {
    step(i) {
      if (timer) timer.stop();
      if (i <= 1) {
        g.transition().duration(600).style("opacity", 1); tg.transition().duration(600).style("opacity", 0);
        timer = d3.timer(() => { rot += 0.08; render(); });
        if (i === 0) { arcs.transition().duration(600).attr("stroke", C.tealDark); legend.transition().duration(400).style("opacity", 0); }
        else { arcs.transition().duration(600).attr("stroke", d => C.region[d.region] || C.grey); legend.transition().duration(400).style("opacity", 1); }
        caption("#c-globe", i === 0 ? "Twenty largest origin countries, arc width proportional to the percentage of Philadelphia's foreign-born. ACS 2022–2024." : "Arcs colored by world region of birth.");
      } else {
        g.transition().duration(600).style("opacity", 0); tg.transition().duration(600).style("opacity", 1);
        drawPath(tl, 1400);
        caption("#c-globe", "Foreign-born population of Philadelphia County, ACS one-year estimates.");
      }
    }
  };
}

// ---------- chapter 2: wage gradient -> decomposition ----------
function initWage() {
  const { svg, w, h } = svgBox("#g-wage");
  const m = { t: 40, r: 40, b: 40, l: 100 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const t1 = state.data.results.t1;
  const y = d3.scaleBand().domain(t1.map(d => d.eng)).range([0, ch * 0.58]).padding(0.45);
  const x = d3.scaleLinear().domain([0, 42]).range([0, cw]);
  axes(g, x, y, cw, ch * 0.58, { xf: d => "$" + d, xt: 6 });
  title(g, "Median hourly wage of foreign-born workers by English, 2022–2024");
  const rows = g.selectAll(".row").data(t1).join("g").attr("class", "row").attr("transform", d => `translate(0,${y(d.eng) + y.bandwidth() / 2})`);
  rows.append("line").attr("x1", d => x(d.lo)).attr("x2", d => x(d.hi)).attr("stroke", C.ink2).attr("stroke-width", 2).style("opacity", 0);
  rows.append("rect").attr("x", 0).attr("y", -y.bandwidth() / 2).attr("height", y.bandwidth()).attr("width", 0).attr("fill", (d, i) => C.english[i]);
  rows.append("text").attr("class", "label").attr("x", d => x(d.median) + 8).attr("dy", 4).text(d => fmt$(d.median)).style("opacity", 0);
  hover(rows, d => `<b>${d.eng}</b><br>median ${fmt$(d.median)}<br>90% interval ${fmt$(d.lo)} to ${fmt$(d.hi)}`);
  const ann = g.append("text").attr("class", "label").attr("x", cw).attr("y", ch * 0.58 + 40).attr("text-anchor", "end").style("opacity", 0)
    .text("holding education, occupation and area constant, +7% per step");
  // decomposition bars
  const oa = state.data.results.oaxaca;
  const dg = g.append("g").attr("transform", `translate(0,${ch * 0.8})`).style("opacity", 0);
  const dy = d3.scaleBand().domain(["Standard controls", "With English"]).range([0, ch * 0.2]).padding(0.4);
  const dx = d3.scaleLinear().domain([0, oa.gap]).range([0, cw]);
  dg.append("text").attr("class", "title").attr("y", -12).text(`Native to foreign-born log wage gap, ${oa.gap.toFixed(3)}`);
  const parts = [["Standard controls", oa.base], ["With English", oa.with_english]];
  parts.forEach(([k, v]) => {
    const yy = dy(k);
    dg.append("rect").attr("x", 0).attr("y", yy).attr("height", dy.bandwidth()).attr("width", dx(v.explained)).attr("fill", C.teal);
    dg.append("rect").attr("x", dx(v.explained)).attr("y", yy).attr("height", dy.bandwidth()).attr("width", dx(v.unexplained)).attr("fill", C.grey2);
    dg.append("text").attr("x", -8).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "end").text(k);
    dg.append("text").attr("class", "label").attr("x", dx(v.explained) / 2).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "middle").attr("fill", "#fff").text(`explained ${Math.round(v.explained / oa.gap * 100)}%`);
    dg.append("text").attr("class", "note").attr("x", dx(v.explained) + dx(v.unexplained) / 2).attr("y", yy + dy.bandwidth() / 2 + 4).attr("text-anchor", "middle").text(`unexplained ${Math.round(v.unexplained / oa.gap * 100)}%`);
  });
  state.charts.wage = {
    step(i) {
      rows.select("rect").transition().duration(T).ease(ease).delay((d, j) => j * 120).attr("width", d => x(d.median));
      rows.select("text").transition().delay(500).duration(400).style("opacity", 1);
      rows.select("line").transition().delay(600).duration(400).style("opacity", i >= 0 ? 1 : 0);
      ann.transition().duration(500).style("opacity", i >= 1 ? 1 : 0);
      dg.transition().duration(600).style("opacity", i >= 2 ? 1 : 0);
      caption("#c-wage", i < 2 ? "Weighted medians with 90 percent replicate-weight intervals. Wage and salary workers aged 25 to 64."
        : "Two-fold Oaxaca decomposition with pooled coefficients, 2022–2024.");
    }
  };
}

// ---------- chapter 3: cohort curves ----------
function initCohort() {
  const { svg, w, h } = svgBox("#g-cohort");
  const m = { t: 40, r: 120, b: 44, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const co = state.data.results.cohort, mx = state.data.results.cohort_max_ysm;
  const names = { pre1990: "Before 1990", "1990s": "1990s", "2000s": "2000s", "2010s": "2010s", "2020s": "2020s" };
  const cols = { pre1990: C.grey2, "1990s": C.grey, "2000s": C.tealLight, "2010s": C.teal, "2020s": C.tealDark };
  const series = Object.keys(names).map(k => ({ k, pts: d3.range(0, Math.min(30, mx[k]) + 1).map(t => ({ t, v: (Math.exp(co[k] + co.ysm * t + co.ysm2 * t * t) - 1) * 100 })) }));
  const x = d3.scaleLinear().domain([0, 30]).range([0, cw]), y = d3.scaleLinear().domain([-26, 4]).range([ch, 0]);
  axes(g, x, y, cw, ch, { yf: d => d + "%" });
  title(g, "Predicted wage gap to natives by years since migration");
  g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", C.axis);
  g.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text("years since migration");
  const line = d3.line().x(d => x(d.t)).y(d => y(d.v)).curve(d3.curveMonotoneX);
  const paths = g.selectAll(".c").data(series).join("path").attr("class", "c").attr("fill", "none").attr("stroke", d => cols[d.k]).attr("stroke-width", 2.4).attr("d", d => line(d.pts));
  hover(paths, d => `<b>Arrived ${names[d.k]}</b><br>entry gap ${d.pts[0].v.toFixed(1)}%<br>after ${d.pts[d.pts.length - 1].t} years ${d.pts[d.pts.length - 1].v.toFixed(1)}%`);
  const labels = g.selectAll(".l").data(series).join("text").attr("class", "label").attr("x", d => x(d.pts[d.pts.length - 1].t) + 8).attr("y", d => y(d.pts[d.pts.length - 1].v) + 4 + (d.k === "1990s" ? -7 : d.k === "pre1990" ? 8 : 0)).text(d => names[d.k]).style("opacity", 0);
  state.charts.cohort = {
    step(i) {
      drawPath(paths, 1400);
      labels.transition().delay(1200).duration(400).style("opacity", 1);
      paths.transition("fade").delay(200).duration(600).attr("stroke-opacity", d => i === 1 ? (["2000s", "2010s", "2020s"].includes(d.k) ? 1 : 0.35) : 1);
      caption("#c-cohort", "From the cohort specification with English, occupation, PUMA and year fixed effects. Curves stop at each cohort's longest observed duration.");
    }
  };
}

// ---------- chapter 4: linked map and scatter ----------
function initMap() {
  const { svg, w, h } = svgBox("#g-map");
  if (!state.pumas) { svg.append("text").attr("x", 20).attr("y", 40).attr("class", "title").text("Upload data/pumas.geojson for the map"); state.charts.map = { step() {} }; return; }
  const chart = CH.linkedMap(svg, state.pumas, { width: w, height: h, stacked: true, mobility: state.data.mobility ? state.data.mobility.flows : null });
  state.charts.map = {
    step(i) {
      chart.step(i === 0 ? 0 : 2);
      caption("#c-map", i === 0 ? "Foreign-born percentage of residents by PUMA. Dots are the same 48 PUMAs against foreign-born median wage, sized by limited English among the foreign-born. Hover or brush to link the two."
        : "Where the foreign-born residents who left the city for a suburb in the past year went, 2022–2024. Circle size is the annual count arriving in each PUMA.");
    }
  };
}

// ---------- chapter 4b: county roses ----------
function initCounty() {
  const { svg, w, h } = svgBox("#g-county");
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

// ---------- chapter 5: density slopes by group ----------
function initSlopes() {
  const { svg, w, h } = svgBox("#g-slopes");
  const m = { t: 44, r: 110, b: 40, l: 210 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const S = state.data.results.slopes;
  const ttl = g.append("text").attr("class", "title").attr("y", -16).text("Change in log wage per SD of area immigrant density");
  const body = g.append("g");
  const lf = d => `${(d.v * 100).toFixed(1)}% (SE ${(d.se * 100).toFixed(1)})`;
  function draw(rows, hl) {
    body.selectAll("*").remove();
    const r = intervalChart(body, rows, cw, Math.min(ch, rows.length * 90), { domain: [-0.14, 0.04], xf: d => (d * 100).toFixed(0) + "%", lf });
    if (hl) r.style("opacity", d => hl.includes(d.g) ? 1 : 0.35);
  }
  const base = [{ g: "Proficient immigrants", ...S.generic[0], c: C.teal }, { g: "Limited-English immigrants", ...S.generic[1], c: C.orange }];
  const full = base.concat([{ g: "Native-born workers", ...S.generic[2], c: C.grey }, { g: "Proficient immigrants, with native wage control", ...S.with_native_wage[0], c: C.tealDark }]);
  state.charts.slopes = {
    step(i) {
      if (i === 0) { draw(base, ["Proficient immigrants"]); caption("#c-slopes", "Two-level model, 7,355 foreign-born workers in 48 PUMAs, 2022–2024. Bars are 95 percent intervals."); }
      else if (i === 1) { draw(base); caption("#c-slopes", `Interaction 0.049 (SE 0.029, p = 0.09 with Satterthwaite degrees of freedom). In 2017–2021, 0.050 (SE 0.028).`); }
      else { draw(full); caption("#c-slopes", `Native-born workers: ${fmtN(state.data.results.natives_n)} in the same 48 PUMAs, density on the same scale.`); }
    }
  };
}

// ---------- chapter 6: enclave lines morph ----------
function initEnclave() {
  const { svg, w, h } = svgBox("#g-enclave");
  const m = { t: 40, r: 40, b: 44, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const E = state.data.results.enclave;
  const x = d3.scaleLinear().domain([-1.5, 2.5]).range([0, cw]), y = d3.scaleLinear().domain([-0.5, 0.15]).range([ch, 0]);
  axes(g, x, y, cw, ch, { yf: d3.format("+.1f") });
  const ttl = title(g, "");
  g.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text("PUMA foreign-born percentage, standardized");
  g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", C.axis);
  const pts = k => d3.range(-1.5, 2.51, 0.1).map(d => ({ d, prof: k.density * d, lim: k.limited + (k.density + k.inter) * d }));
  const line = key => d3.line().x(p => x(p.d)).y(p => y(p[key])).curve(d3.curveLinear);
  const prof = g.append("path").attr("fill", "none").attr("stroke", C.teal).attr("stroke-width", 2.8);
  const lim = g.append("path").attr("fill", "none").attr("stroke", C.orange).attr("stroke-width", 2.8);
  const gap = g.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("stroke", C.ink).attr("stroke-dasharray", "3 3");
  const gapT = g.append("text").attr("class", "label").attr("x", x(0) + 8);
  const lp = g.append("text").attr("class", "label").attr("x", x(-1.5)).attr("fill", C.tealDark).text("Proficient English");
  const ll = g.append("text").attr("class", "label").attr("x", x(-1.5)).attr("fill", C.orange).text("Limited English");
  function render(k, t) {
    const p = pts(k);
    prof.transition().duration(T).ease(ease).attr("d", line("prof")(p));
    lim.transition().duration(T).ease(ease).attr("d", line("lim")(p));
    gap.transition().duration(T).attr("y1", y(0)).attr("y2", y(k.limited));
    gapT.transition().duration(T).attr("y", y(k.limited / 2) + 4).text(`differential at mean density ${Math.round((1 - Math.exp(k.limited)) * 100)}%`);
    lp.transition().duration(T).attr("y", y(p[0].prof) - 10); ll.transition().duration(T).attr("y", y(p[0].lim) - 10);
    ttl.text(t);
  }
  state.charts.enclave = {
    step(i) {
      if (i === 0) { render(E.A, "Fitted log wage by area density, no occupation controls"); caption("#c-enclave", "Two-level model. Log wage relative to a proficient worker at mean density."); }
      else { render(E.B, "Fitted log wage by area density, within occupations"); caption("#c-enclave", "Same specification with four-digit occupation fixed effects, standard errors clustered on PUMA."); }
    }
  };
}

// ---------- chapter 7: own-language density ----------
function initOwnlang() {
  const { svg, w, h } = svgBox("#g-ownlang");
  const m = { t: 44, r: 120, b: 40, l: 230 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const O = state.data.results.ownlang;
  g.append("text").attr("class", "title").attr("y", -16).text("Change in log wage per SD of own-language density");
  const body = g.append("g");
  const lf = d => `${(d.v * 100).toFixed(1)}% (SE ${(d.se * 100).toFixed(1)})`;
  const rowsFor = (city, tag) => [{ g: `${tag}proficient`, v: O[city].prof, se: O[city].prof_se, c: C.teal }, { g: `${tag}limited English`, v: O[city].lim, se: O[city].lim_se, c: C.orange }];
  function draw(rows, dim) {
    body.selectAll("*").remove();
    const r = intervalChart(body, rows, cw, Math.min(ch, rows.length * 80), { domain: [-0.1, 0.04], xf: d => (d * 100).toFixed(0) + "%", lf });
    if (dim) r.style("opacity", 0.25);
  }
  state.charts.ownlang = {
    step(i) {
      if (i === 0) { draw(rowsFor("Philadelphia", "Philadelphia, "), true); caption("#c-ownlang", `Exploratory. ${fmtN(O.Philadelphia.n)} workers with a non-English home language. One SD of own-language density is ${O.Philadelphia.sd_pp} percentage points.`); }
      else if (i === 1) { draw(rowsFor("Philadelphia", "Philadelphia, ")); caption("#c-ownlang", `Interaction ${O.Philadelphia.inter} (SE ${O.Philadelphia.inter_se}). Generic density is held constant in the same model.`); }
      else { draw(rowsFor("Philadelphia", "Philadelphia, ").concat(rowsFor("New York", "New York, "))); caption("#c-ownlang", `New York: ${fmtN(O["New York"].n)} workers, one SD is ${O["New York"].sd_pp} points, interaction ${O["New York"].inter} (SE ${O["New York"].inter_se}).`); }
    }
  };
}

// ---------- chapter 8: one year of moves ----------
function initMoves() {
  const { svg, w, h } = svgBox("#g-moves");
  const chart = CH.moveFlow(svg, state.data.moveRates, { width: w, height: h });
  state.charts.moves = {
    step(i) {
      chart.step(i);
      caption("#c-moves", i === 0 ? "Where foreign-born adults aged 25 to 64 lived a year earlier and where they live now, 2022–2024. Weighted percentages of the group."
        : i === 1 ? "The same year with the 85.7 percent who did not move removed. Ribbon color is the movers' median hourly wage where the paper reports it."
        : "Native-born adults for comparison. City-to-suburb and suburb-to-city rates are close to the foreign-born rates.");
    }
  };
}

// ---------- chapter 9: pseudo-panel and duration-matched ----------
function initPanel() {
  const { svg, w, h } = svgBox("#g-panel");
  const m = { t: 40, r: 40, b: 44, l: 60 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const cells = state.data.results.cells.map(d => ({ ...d, suburb_prop: +d.suburb_prop, median_hourly: +d.median_hourly }));
  const D = state.data.results.duration;
  const A = g.append("g"), B = g.append("g").style("opacity", 0);
  // A: trajectories
  const x = d3.scaleLinear().domain([10, 45]).range([0, cw]), y = d3.scaleLinear().domain([50, 90]).range([ch, 0]);
  axes(A, x, y, cw, ch, { xf: d => "$" + d, yf: d => d + "%" });
  A.append("text").attr("class", "title").attr("y", -14).text("Suburban residence and median wage by cohort and region, three windows");
  A.append("text").attr("x", cw / 2).attr("y", ch + 36).attr("text-anchor", "middle").text("median hourly wage, 2024 dollars");
  const byCell = d3.groups(cells, d => d.cell).map(([k, v]) => ({ k, region: v[0].region, cohort: v[0].cohort, pts: v.sort((a, b) => a.window.localeCompare(b.window)) }));
  const line = d3.line().x(d => x(d.median_hourly)).y(d => y(d.suburb_prop));
  const paths = A.selectAll(".tr").data(byCell).join("path").attr("class", "tr").attr("fill", "none").attr("stroke", d => C.region[d.region]).attr("stroke-width", d => d.cohort === "2010s" ? 2.6 : 1.4).attr("stroke-opacity", 0.9).attr("d", d => line(d.pts));
  hover(paths, d => `<b>${d.region}, arrived ${d.cohort === "pre1990" ? "before 1990" : d.cohort}</b><br>` + d.pts.map(p => `${p.window}: ${fmt$(p.median_hourly)}, ${p.suburb_prop.toFixed(0)}% suburban`).join("<br>"));
  paths.on("mouseenter", function () { paths.attr("stroke-opacity", 0.15); d3.select(this).attr("stroke-opacity", 1).attr("stroke-width", 3.2); })
       .on("mouseleave.restore", function (ev, d) { paths.attr("stroke-opacity", 0.9).attr("stroke-width", d => d.cohort === "2010s" ? 2.6 : 1.4); });
  A.selectAll(".end").data(byCell).join("circle").attr("cx", d => x(d.pts[d.pts.length - 1].median_hourly)).attr("cy", d => y(d.pts[d.pts.length - 1].suburb_prop)).attr("r", d => d.cohort === "2010s" ? 5 : 3).attr("fill", d => C.region[d.region]);
  const leg = A.append("g").attr("transform", `translate(${cw - 120},${10})`);
  Object.entries(C.region).forEach(([k, c], i) => { leg.append("line").attr("x1", 0).attr("x2", 16).attr("y1", i * 18).attr("y2", i * 18).attr("stroke", c).attr("stroke-width", 2.5); leg.append("text").attr("x", 22).attr("y", i * 18 + 4).text(k); });
  A.append("text").attr("class", "note").attr("x", 0).attr("y", ch + 36).text("thick lines: 2010s arrivals");
  // B: duration-matched dumbbells
  const yb = d3.scaleBand().domain(D.map(d => d.region)).range([0, Math.min(ch, 320)]).padding(0.5);
  const xb = d3.scaleLinear().domain([50, 80]).range([0, cw]);
  B.append("text").attr("class", "title").attr("y", -14).text("Percent living outside the city at matched duration of residence");
  B.append("g").attr("class", "grid").call(d3.axisLeft(yb).tickSize(-cw).tickFormat(""));
  B.append("g").attr("class", "axis").attr("transform", `translate(0,${yb.range()[1]})`).call(d3.axisBottom(xb).ticks(6).tickFormat(d => d + "%").tickSizeOuter(0));
  const rb = B.selectAll(".db").data(D).join("g").attr("transform", d => `translate(0,${yb(d.region) + yb.bandwidth() / 2})`);
  rb.append("text").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").attr("class", "label").text(d => d.region);
  rb.append("line").attr("x1", d => xb(d.c2000)).attr("x2", d => xb(d.c2010)).attr("stroke", C.grey2).attr("stroke-width", 4);
  rb.append("circle").attr("cx", d => xb(d.c2000)).attr("r", 6).attr("fill", C.grey);
  rb.append("circle").attr("cx", d => xb(d.c2010)).attr("r", 6).attr("fill", d => Math.abs(d.diff / d.se) >= 1.96 ? C.orange : C.gold);
  rb.append("text").attr("class", "label").attr("x", d => xb(Math.max(d.c2000, d.c2010)) + 12).attr("dy", 4).text(d => `${d.diff > 0 ? "+" : ""}${d.diff} points (SE ${d.se})`);
  const lb = B.append("g").attr("transform", `translate(0,${yb.range()[1] + 50})`);
  lb.append("circle").attr("r", 5).attr("fill", C.grey); lb.append("text").attr("x", 10).attr("dy", 4).text("2000s cohort in 2012–2016");
  lb.append("circle").attr("cx", 230).attr("r", 5).attr("fill", C.orange); lb.append("text").attr("x", 240).attr("dy", 4).text("2010s cohort in 2022–2024");
  state.charts.panel = {
    step(i) {
      if (i === 0) { A.transition().duration(500).style("opacity", 1); B.transition().duration(500).style("opacity", 0); drawPath(paths, 1200); caption("#c-panel", "Sixteen cohort-by-region cells, 44 cell-window observations. Each path runs from 2012–2016 to 2022–2024."); }
      else { A.transition().duration(500).style("opacity", 0); B.transition().duration(500).style("opacity", 1); caption("#c-panel", "Replicate-weight standard errors. Orange marks differences of about two standard errors or more."); }
    }
  };
}

// ---------- chapter 7b: every interaction term ----------
function initCompare() {
  const { svg, w, h } = svgBox("#g-compare");
  const chart = CH.interactions(svg, CH.INTERACTIONS(state.data.interactionsRegion), { width: w, height: h });
  state.charts.compare = {
    step(i) {
      chart.step(i);
      caption("#c-compare", i === 0 ? "Every limited-English × density interaction reported in the paper, sorted by |t|. Bars are 95 percent intervals. Hover a point for the estimate, the standard error and the sample."
        : "The two New York own-language terms are the only ones beyond the Bonferroni line for this many tests.");
    }
  };
}

// ---------- chapter 6b: occupation matrix ----------
function initOcc() {
  const { svg, w, h } = svgBox("#g-occ");
  if (!state.data.occupations) { state.charts.occ = { step() {} }; return; }
  const chart = CH.occMatrix(svg, state.data.occupations, { width: w, height: h });
  state.charts.occ = {
    step(i) {
      chart.step(i);
      caption("#c-occ", i === 0 ? "Percentage of each group in twelve occupation groups, 2022–2024 wage and salary workers aged 25 to 64. Color is the group's median hourly wage in that occupation. The right column divides the limited-English median by the proficient median in the same occupation."
        : "Sorted by the native-born wage. The occupations at the top hold most proficient immigrants and few limited-English workers.");
    }
  };
}

// ---------- chapter 10: cell heatmap ----------
function initGrid() {
  const { svg, w, h } = svgBox("#g-grid");
  const m = { t: 50, r: 40, b: 30, l: 210 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
  const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
  const cells = state.data.results.cells.map(d => ({ ...d, suburb_prop: +d.suburb_prop, median_hourly: +d.median_hourly }));
  const regions = ["Asia", "Latin America", "Europe", "Africa"], cohorts = ["pre1990", "1990s", "2000s", "2010s"], windows = ["2012-2016", "2017-2021", "2022-2024"];
  const cn = { pre1990: "before 1990", "1990s": "1990s", "2000s": "2000s", "2010s": "2010s" };
  const rows = []; regions.forEach(r => cohorts.forEach(c => rows.push(`${r} · ${cn[c]}`)));
  const rowKey = d => `${d.region} · ${cn[d.cohort]}`;
  const y = d3.scaleBand().domain(rows).range([0, Math.min(ch, rows.length * 34)]).padding(0.1);
  const x = d3.scaleBand().domain(windows).range([0, Math.min(cw, 420)]).padding(0.08);
  const ttl = g.append("text").attr("class", "title").attr("y", -30);
  windows.forEach(wn => g.append("text").attr("class", "label").attr("x", x(wn) + x.bandwidth() / 2).attr("y", -10).attr("text-anchor", "middle").text(wn.replace("-", "–")));
  rows.forEach((r, i) => g.append("text").attr("x", -12).attr("y", y(r) + y.bandwidth() / 2 + 4).attr("text-anchor", "end").attr("fill", C.region[r.split(" · ")[0]]).text(r));
  regions.slice(1).forEach(r => { const yy = y(`${r} · before 1990`) - y.step() * y.padding() / 2; g.append("line").attr("x1", -200).attr("x2", x.range()[1]).attr("y1", yy).attr("y2", yy).attr("stroke", C.grid); });
  const cell = g.selectAll(".hc").data(cells).join("g").attr("class", "hc").attr("transform", d => `translate(${x(d.window)},${y(rowKey(d))})`);
  const rect = cell.append("rect").attr("width", x.bandwidth()).attr("height", y.bandwidth()).attr("rx", 2).attr("fill", "#eee").attr("stroke", "#fff");
  const txt = cell.append("text").attr("x", x.bandwidth() / 2).attr("y", y.bandwidth() / 2 + 4).attr("text-anchor", "middle").attr("class", "label");
  hover(cell, d => `<b>${d.region}, arrived ${cn[d.cohort]}, ${d.window.replace("-", "–")}</b><br>median wage ${fmt$(d.median_hourly)} (2024 dollars)<br>${d.suburb_prop.toFixed(1)}% living outside the city<br>${(+d.limited_eng_prop).toFixed(0)}% limited English · n = ${fmtN(+d.n_unweighted)}`);
  const lg = g.append("g").attr("transform", `translate(${x.range()[1] + 30},0)`);
  function paint(key, interp, fmt, t) {
    ttl.text(t);
    const ext = d3.extent(cells, d => d[key]); const sc = d3.scaleSequential(interp).domain(ext);
    rect.transition().duration(T).attr("fill", d => sc(d[key]));
    txt.transition().duration(T).attr("fill", d => (d[key] - ext[0]) / (ext[1] - ext[0]) > 0.6 ? "#fff" : C.ink).text(d => fmt(d[key]));
    lg.selectAll("*").remove();
    const ls = d3.scaleLinear().domain(ext).range([120, 0]);
    d3.range(0, 121, 4).forEach(v => lg.append("rect").attr("x", 0).attr("y", v).attr("width", 10).attr("height", 4).attr("fill", sc(ls.invert(v))));
    lg.append("text").attr("x", 16).attr("y", 8).text(fmt(ext[1])); lg.append("text").attr("x", 16).attr("y", 122).text(fmt(ext[0]));
  }
  state.charts.grid = {
    step(i) {
      if (i === 0) { paint("median_hourly", d3.interpolate("#f6efd9", "#7a5a10"), d => "$" + d.toFixed(0), "Real median hourly wage by cohort, region and window"); caption("#c-grid", "Cells are arrival cohort by region of birth. The 2010s cohort is not observed in 2012–2016. Wages in 2024 dollars."); }
      else { paint("suburb_prop", d3.interpolate("#dff4f4", "#0a4f55"), d => d.toFixed(0) + "%", "Percent living outside Philadelphia County by cohort, region and window"); caption("#c-grid", "Weighted percentages of foreign-born adults aged 25 to 64."); }
    }
  };
}

// ---------- boot ----------
async function boot() {
  const [trend, origins, world, results] = await Promise.all([
    d3.csv("data/trend.csv"), d3.csv("data/origins.csv"), d3.json("data/land-110m.json"), d3.json("data/results.json")
  ]);
  state.data = { trend, origins, results };
  state.world = topojson.feature(world, world.objects.land);
  try { state.pumas = await d3.json("data/pumas.geojson"); } catch (e) { state.pumas = null; }
  try { state.data.mobility = await d3.json("data/mobility.json"); } catch (e) { state.data.mobility = null; }
  try { state.data.occupations = await d3.csv("data/occupations.csv"); } catch (e) { state.data.occupations = null; }
  try { state.data.counties = await d3.csv("data/county_profiles.csv"); } catch (e) { state.data.counties = null; }
  try { const mr = await d3.csv("data/move_rates.csv"); state.data.moveRates = mr.map(d => ({ ...d, fb: d.fb === "TRUE" || d.fb === "true", n: +d.n, pct: +d.pct })); }
  catch (e) { state.data.moveRates = state.data.mobility ? state.data.mobility.move_rates.map(d => ({ ...d, fb: true })) : []; }
  try { const ir = await d3.csv("data/interactions_region.csv"); state.data.interactionsRegion = ir.filter(d => d.Estimate).map(d => ({ region: d.sample, est: d.Estimate, se: d["Std. Error"] })); } catch (e) { state.data.interactionsRegion = []; }
  const inits = { globe: initGlobe, wage: initWage, cohort: initCohort, map: initMap, county: initCounty, slopes: initSlopes, enclave: initEnclave, occ: initOcc, ownlang: initOwnlang, compare: initCompare, moves: initMoves, panel: initPanel, grid: initGrid };
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
