/* Story charts for chapters 6, 7, 9 and 10. D3 v7. Every number is from the paper's tables
   (results.json: slopes, enclave, ownlang, coef_grid; tables.json: tab3, tab4). */
(function () {
  const K = CH.K, hover = CH.hover, T = 700;
  const pct = v => (Math.exp(v) - 1) * 100;                     // log points -> percent difference
  const fp = d3.format("+.0f"), fp1 = d3.format("+.1f"), f3 = d3.format("+.3f");
  const halo = t => t.attr("stroke", "#fff").attr("stroke-width", 4).attr("stroke-linejoin", "round").attr("paint-order", "stroke");

  // ---------- chapter 6: the density gradient, unpacked ----------
  // opts: { width, height, S: results.slopes, limited: enclave.A.limited, sd: 5.4, mean: 12 }
  CH.gradient = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, S = opts.S, sd = opts.sd || 5.4, mean = opts.mean || 12;
    const side = Math.min(250, w * 0.26), m = { t: 54, r: side + 30, b: 64, l: 64 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLinear().domain([-1.5, 3]).range([0, cw]), y = d3.scaleLinear().domain([-36, 14]).range([ch, 0]);
    const prof = { b: S.generic[0].v, se: S.generic[0].se }, lim = { a: opts.limited, b: S.generic[1].v, se: S.generic[1].se }, nat = { b: S.generic[2].v, se: S.generic[2].se }, ctl = { b: S.with_native_wage[0].v, se: S.with_native_wage[0].se };
    const inter = opts.inter; // { v2022, se2022, p2022, v2017, se2017 }
    const p17 = { a: -0.217, prof: -0.066, lim: -0.016 };        // Table 2 column 2, 2017–2021, 44 PUMAs
    const pts = d3.range(-1.5, 3.001, 0.05);
    const ttl = g.append("text").attr("class", "title").attr("y", -30).text("Fitted foreign-born wage by area immigrant density, 2022–2024");
    g.append("text").attr("class", "note").attr("y", -14).text("two-level model, 7,355 workers in 48 PUMAs · percent difference from a proficient immigrant in a PUMA of average density · bands: 95 percent interval of the slope");
    g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(6).tickSize(-cw).tickFormat(""));
    g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(6).tickFormat(d => fp(d) + "%").tickSizeOuter(0)).select(".domain").remove();
    const xa = g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([-1, 0, 1, 2, 3]).tickFormat(d => (d === 0 ? "mean" : fp(d) + " SD")).tickSizeOuter(0));
    xa.selectAll(".tick").append("text").attr("class", "note").attr("y", 26).attr("fill", K.muted).attr("text-anchor", "middle").text(d => (mean + sd * d).toFixed(0) + (d === 0 ? "% foreign-born" : "%"));
    g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", K.axis);
    g.append("text").attr("x", cw / 2).attr("y", ch + 52).attr("text-anchor", "middle").attr("class", "label").text(`immigrant density of the PUMA, standard deviations from the mean (1 SD = ${sd} percentage points)`);
    const line = f => d3.line().x(d => x(d)).y(d => y(pct(f(d)))), area = (lo, hi) => d3.area().x(d => x(d)).y0(d => y(pct(lo(d)))).y1(d => y(pct(hi(d))));
    const fan = (a, b, se) => [d => a + b * d - 1.96 * se * Math.abs(d), d => a + b * d + 1.96 * se * Math.abs(d)];
    const L = {};
    function series(id, a, b, se, color, opts2 = {}) {
      const grp = g.append("g").attr("class", "s-" + id).style("opacity", 0);
      if (se != null) { const [lo, hi] = fan(a, b, se); grp.append("path").datum(pts).attr("d", area(lo, hi)).attr("fill", color).attr("fill-opacity", opts2.dashed ? 0 : 0.12); }
      grp.append("path").datum(pts).attr("d", line(d => a + b * d)).attr("fill", "none").attr("stroke", color).attr("stroke-width", opts2.dashed ? 1.6 : 2.8).attr("stroke-dasharray", opts2.dashed ? "5 4" : null);
      const ex = 2.95, t = grp.append("text").attr("class", "note").attr("x", x(ex)).attr("y", y(pct(a + b * ex)) - 6).attr("text-anchor", "end").attr("fill", color).style("font-weight", 700).text(opts2.tag);
      halo(t);
      L[id] = { grp, f: d => a + b * d, color, label: opts2.label, tag: t, y0: y(pct(a + b * ex)) - 6, se, b };
      return grp;
    }
    // gap shading between the two immigrant lines, drawn under the lines
    const gapArea = g.append("path").datum(pts).attr("d", area(d => lim.a + lim.b * d, d => prof.b * d)).attr("fill", K.orange).attr("fill-opacity", 0.07).style("opacity", 0);
    series("nat", 0, nat.b, nat.se, K.grey, { tag: "native-born", label: `native-born workers, ${fp1(nat.b * 100)}% per SD (SE ${(nat.se * 100).toFixed(1)})` });
    series("p17", 0, p17.prof, null, K.tealDark, { dashed: true, tag: "proficient, 2017–2021", label: `proficient, 2017–2021 window, ${fp1(p17.prof * 100)}% per SD` });
    series("l17", p17.a, p17.lim, null, K.orange, { dashed: true, tag: "limited English, 2017–2021", label: `limited English, 2017–2021 window, ${fp1(p17.lim * 100)}% per SD` });
    series("ctl", 0, ctl.b, null, K.tealDark, { dashed: true, tag: "proficient, area wage held constant", label: `proficient, area's native wage held constant, ${fp1(ctl.b * 100)}% per SD (SE ${(ctl.se * 100).toFixed(1)})` });
    series("prof", 0, prof.b, prof.se, K.teal, { tag: "proficient immigrants", label: `proficient immigrants, ${fp1(prof.b * 100)}% per SD (SE ${(prof.se * 100).toFixed(1)})` });
    series("lim", lim.a, lim.b, lim.se, K.orange, { tag: "limited English", label: `limited-English immigrants, ${fp1(lim.b * 100)}% per SD (SE ${(lim.se * 100).toFixed(1)})` });
    // differential labels at whole SDs
    const gapG = g.append("g").style("opacity", 0);
    [-1, 0, 1, 2].forEach(d => {
      const yp = y(pct(prof.b * d)), yl = y(pct(lim.a + lim.b * d));
      gapG.append("line").attr("x1", x(d)).attr("x2", x(d)).attr("y1", yp).attr("y2", yl).attr("stroke", K.orange).attr("stroke-width", 1).attr("stroke-dasharray", "2 3");
      const t = gapG.append("text").attr("class", "label").attr("x", x(d) + (d >= 1 ? -6 : 6)).attr("text-anchor", d >= 1 ? "end" : "start").attr("y", (yp + yl) / 2 + 4).attr("fill", K.orange).style("font-weight", 700).text(`${((1 - Math.exp(lim.a + (lim.b - prof.b) * d)) * 100).toFixed(0)}% gap`);
      halo(t);
    });
    const it = gapG.append("text").attr("class", "note").attr("x", x(-1.45)).attr("y", y(pct(lim.a + lim.b * -1.45)) + 40).attr("fill", K.ink2)
      .text(`gap change per SD (the interaction): ${f3(inter.v2022)}, SE ${inter.se2022.toFixed(3)}, p = ${inter.p2022}`);
    halo(it);
    // right panel: reading at the cursor, and the decomposition of the gradient
    const side0 = m.l + cw + 30, panel = svg.append("g").attr("transform", `translate(${side0},${m.t})`);
    const read = panel.append("g");
    const decomp = panel.append("g").attr("transform", `translate(0,${ch - 128})`).style("opacity", 0);
    (function () {
      decomp.append("text").attr("class", "label").style("font-weight", 700).text("Is it the area or the immigrants?");
      const bw = side - 10, sc = d3.scaleLinear().domain([0, -prof.b]).range([0, bw]);
      const rows = [
        { k: "shared with natives, or absorbed by the area's native wage level", v: -prof.b - (-ctl.b), c: K.grey },
        { k: "specific to the foreign-born", v: -ctl.b, c: K.teal }];
      let x0 = 0;
      decomp.append("text").attr("class", "note").attr("y", 18).text(`proficient gradient, ${(prof.b * 100).toFixed(1)} percent per SD, split as`);
      rows.forEach(r => { decomp.append("rect").attr("x", sc(x0)).attr("y", 28).attr("width", sc(r.v)).attr("height", 16).attr("fill", r.c); x0 += r.v; });
      decomp.append("text").attr("class", "note").attr("y", 60).attr("fill", K.ink2).text(`about a quarter (−${(( -prof.b + ctl.b) * 100).toFixed(1)} points): ${rows[0].k.split(",")[0]},`);
      decomp.append("text").attr("class", "note").attr("y", 74).attr("fill", K.ink2).text("or absorbed by the area's native wage level");
      decomp.append("text").attr("class", "note").attr("y", 92).attr("fill", K.tealDark).text(`the rest (−${(-ctl.b * 100).toFixed(1)} points): specific to the foreign-born`);
      decomp.append("text").attr("class", "note").attr("y", 112).attr("fill", K.muted).text("native-born workers in the same PUMAs: −2.0% per SD (SE 1.1)");
      decomp.append("text").attr("class", "note").attr("y", 126).attr("fill", K.muted).text("with the area's native wage held constant: −6.4% (SE 1.3)");
    })();
    // cursor rule
    const rule = g.append("g").style("pointer-events", "none");
    rule.append("line").attr("y1", 0).attr("y2", ch).attr("stroke", K.ink).attr("stroke-width", 1).attr("stroke-dasharray", "3 3");
    const dots = rule.append("g");
    let shown = new Set(), curD = 1;
    function reading(d) {
      curD = d; rule.attr("transform", `translate(${x(d)},0)`);
      const vis = ["prof", "lim", "nat", "ctl"].filter(k => shown.has(k));
      dots.selectAll("circle").data(vis, k => k).join("circle").attr("cy", k => y(pct(L[k].f(d)))).attr("r", 5).attr("fill", k => L[k].color).attr("stroke", "#fff");
      read.selectAll("*").remove();
      read.append("text").attr("class", "label").style("font-weight", 700).text(`At ${d >= 0 ? "+" : ""}${d.toFixed(1)} SD, a PUMA ${(mean + sd * d).toFixed(0)} percent foreign-born`);
      const names = { prof: "proficient immigrants", lim: "limited-English immigrants", nat: "native-born", ctl: "proficient, area wage held constant" };
      let yy = 24;
      vis.forEach(k => { read.append("circle").attr("cx", 5).attr("cy", yy - 4).attr("r", 4).attr("fill", L[k].color); read.append("text").attr("class", "note").attr("x", 14).attr("y", yy).attr("fill", K.ink).style("font-weight", 700).text(`${names[k]}: ${fp(pct(L[k].f(d)))}%`); read.append("text").attr("class", "note").attr("x", 14).attr("y", yy + 13).attr("fill", K.muted).text(L[k].label.replace(/^[^,]*, /, "")); yy += 32; });
      if (shown.has("lim")) { yy += 4; read.append("text").attr("class", "note").attr("x", 0).attr("y", yy).attr("fill", K.orange).style("font-weight", 700).text(`limited-English gap here: ${((1 - Math.exp(lim.a + (lim.b - prof.b) * d)) * 100).toFixed(0)} percent`); yy += 17; }
      read.append("text").attr("class", "note").attr("x", 0).attr("y", yy + 6).attr("fill", K.muted).text("move the mouse across the chart");
    }
    g.append("rect").attr("width", cw).attr("height", ch).attr("fill", "transparent").on("mousemove", ev => reading(Math.round(x.invert(d3.pointer(ev)[0]) * 10) / 10));
    function show(keys, extra = {}) {
      shown = new Set(keys);
      Object.entries(L).forEach(([k, o]) => o.grp.transition().duration(T).style("opacity", shown.has(k) ? (extra.dim && extra.dim.includes(k) ? 0.45 : 1) : 0));
      gapArea.transition().duration(T).style("opacity", shown.has("lim") ? 1 : 0); gapG.transition().duration(T).style("opacity", shown.has("lim") ? 1 : 0);
      decomp.transition().duration(T).style("opacity", extra.decomp ? 1 : 0);
      const vis = Object.entries(L).filter(([k]) => shown.has(k)).map(([k, o]) => ({ k, o, y: o.y0 })).sort((a, b) => a.y - b.y);
      for (let i = 1; i < vis.length; i++) if (vis[i].y - vis[i - 1].y < 13) vis[i].y = vis[i - 1].y + 13;
      vis.forEach(v => v.o.tag.transition().duration(T).attr("y", v.y));
      reading(curD);
    }
    return { show, step(i) { if (i <= 1) show(["prof"]); else if (i === 2) show(["prof", "lim", "p17", "l17"], { dim: ["p17", "l17"] }); else show(["prof", "lim", "nat", "ctl"], { decomp: true }); } };
  };

  // ---------- chapter 5, first half: the English ladder beside the density scatter ----------
  // opts: { width, height, pumas, t1: [{eng, median, lo, hi}], oaxaca, engColors }
  CH.wageScatter = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, sw = Math.round(w * 0.58);
    const gS = svg.append("g");
    const sc = CH.linkedMap(gS, opts.pumas, { width: sw, height: h, mode: "scatter", title: "Wage by immigrant density, 48 PUMAs, r = −0.37" });
    sc.trend(true);
    const y = sc.y, m = sc.m, ch = y.range()[0];
    // ladder: the four English categories on the same wage axis
    const lx0 = sw + 30, lw = w - lx0 - 20, gl = svg.append("g").attr("transform", `translate(${lx0},${m.t})`);
    const t1 = opts.t1, x = d3.scalePoint().domain(t1.map(d => d.eng)).range([28, lw - 40]);
    gl.append("text").attr("class", "title").attr("y", -16).text("By English, 2022–2024");
    gl.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(5).tickSize(-lw).tickFormat(""));
    gl.append("line").attr("x1", 0).attr("x2", 0).attr("y1", 0).attr("y2", ch).attr("stroke", K.axis);
    const rows = gl.selectAll("g.r").data(t1).join("g").attr("class", "r").attr("transform", d => `translate(${x(d.eng)},0)`);
    rows.append("line").attr("y1", d => y(d.lo)).attr("y2", d => y(d.hi)).attr("stroke", (d, i) => opts.engColors[i]).attr("stroke-width", 3).attr("stroke-linecap", "round");
    rows.append("circle").attr("cy", d => y(d.median)).attr("r", 7).attr("fill", (d, i) => opts.engColors[i]).attr("stroke", "#fff").attr("stroke-width", 1.5);
    halo(rows.append("text").attr("class", "label").attr("x", 11).attr("y", d => y(d.median) + 4).style("font-weight", 700).text(d => "$" + d.median.toFixed(0)));
    rows.append("text").attr("class", "note").attr("y", ch + 16).attr("text-anchor", "middle").attr("fill", K.ink).text(d => d.eng.replace("Not at all", "none").replace("Not well", "not well").replace("Very well", "very well").replace("Well", "well"));
    gl.append("text").attr("class", "note").attr("x", lw / 2).attr("y", ch + 32).attr("text-anchor", "middle").attr("fill", K.muted).text("speaks English · 90 percent intervals");
    hover(rows, d => `<b>Speaks English ${d.eng.toLowerCase()}</b><br>median ${d3.format("$.2f")(d.median)}<br>90% interval ${d3.format("$.0f")(d.lo)} to ${d3.format("$.0f")(d.hi)}`);
    // step 1: the model annotation, drawn as the step between adjacent categories
    const ann = gl.append("g").style("opacity", 0);
    const py = y(t1[3].median) - 44;
    ["holding cohort, education,", "occupation and area constant:", "+0.059 log points per step, about 6%"].forEach((t, i) => ann.append("text").attr("class", "note").attr("x", 6).attr("y", py + i * 14).attr("fill", K.tealDark).style("font-weight", 700).text(t));
    // step 2: decomposition inset, below the ladder
    const oa = opts.oaxaca, gd = svg.append("g").attr("transform", `translate(${lx0 + 6},${m.t + 16})`).style("opacity", 0);
    const bw = lw - 10, dx = d3.scaleLinear().domain([0, oa.gap]).range([0, bw]);
    gd.append("text").attr("class", "label").style("font-weight", 700).attr("y", 0).text(`Native to foreign-born gap, ${oa.gap.toFixed(3)} log points`);
    [["standard controls", oa.base], ["with English added", oa.with_english]].forEach(([k, v], i) => {
      const yy = 12 + i * 40;
      gd.append("text").attr("class", "note").attr("x", 0).attr("y", yy + 8).attr("fill", K.ink2).text(`${k}: `).append("tspan").attr("fill", K.tealDark).style("font-weight", 700).text(`explained ${Math.round(v.explained / oa.gap * 100)}%`).append("tspan").attr("fill", K.muted).style("font-weight", 400).text(`, unexplained ${Math.round(v.unexplained / oa.gap * 100)}%`);
      gd.append("rect").attr("x", 0).attr("y", yy + 14).attr("width", dx(v.explained)).attr("height", 10).attr("fill", K.teal);
      gd.append("rect").attr("x", dx(v.explained)).attr("y", yy + 14).attr("width", dx(v.unexplained)).attr("height", 10).attr("fill", K.grey2);
    });
    function step(i) {
      sc.dim(i < 3); sc.trend(i >= 3);
      gl.transition().duration(T).style("opacity", i >= 3 ? 0.35 : 1);
      ann.transition().duration(T).style("opacity", i >= 1 && i < 3 ? 1 : 0);
      gd.transition().duration(T).style("opacity", i === 2 ? 1 : 0);
    }
    return { step };
  };

  // ---------- chapter 7: the occupational channel, four panels, one slider ----------
  // panels: [{ title, gap, prof, lim, sd, note }], opts.d current density in SD
  CH.occPanels = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, P = opts.panels, cols = 2, rows = 2;
    const gapX = 40, gapY = 46, top = 30, pw = (w - gapX - 20) / cols, ph = (h - top - gapY - 24) / rows;
    const m = { t: 50, r: 14, b: 34, l: 48 }, cw = pw - m.l - m.r, ch = ph - m.t - m.b;
    const x = d3.scaleLinear().domain([-1.5, 2.5]).range([0, cw]), y = d3.scaleLinear().domain([-32, 10]).range([ch, 0]);
    const pts = d3.range(-1.5, 2.51, 0.1);
    svg.append("text").attr("class", "title").attr("x", 0).attr("y", 18).text("Fitted wage by area immigrant density, with and without occupation held constant");
    const panels = P.map((S, i) => {
      const px = (i % cols) * (pw + gapX), py = top + Math.floor(i / cols) * (ph + gapY);
      const g = svg.append("g").attr("transform", `translate(${px + m.l},${py + m.t})`).attr("class", "panel");
      g.append("text").attr("class", "label").style("font-weight", 700).attr("y", -34).text(S.title);
      g.append("text").attr("class", "note").attr("y", -20).attr("fill", K.muted).text(S.note);
      const nt = g.append("text").attr("class", "note").attr("y", -6);
      nt.append("tspan").attr("fill", K.tealDark).style("font-weight", 700).text(`proficient ${fp1(S.prof * 100)}% per SD`);
      nt.append("tspan").attr("fill", K.muted).text("  ·  ");
      nt.append("tspan").attr("fill", K.orange).style("font-weight", 700).text(`limited English ${fp1(S.lim * 100)}% per SD`);
      g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(4).tickSize(-cw).tickFormat(""));
      g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(4).tickFormat(d => fp(d) + "%").tickSizeOuter(0)).select(".domain").remove();
      g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([-1, 0, 1, 2]).tickFormat(d => d === 0 ? "mean" : fp(d) + " SD").tickSizeOuter(0));
      g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", K.axis);
      g.append("path").datum(pts).attr("d", d3.area().x(d => x(d)).y0(d => y(pct(S.gap + S.lim * d))).y1(d => y(pct(S.prof * d)))).attr("fill", K.orange).attr("fill-opacity", 0.08);
      g.append("path").datum(pts).attr("d", d3.line().x(d => x(d)).y(d => y(pct(S.prof * d)))).attr("fill", "none").attr("stroke", K.teal).attr("stroke-width", 2.6);
      g.append("path").datum(pts).attr("d", d3.line().x(d => x(d)).y(d => y(pct(S.gap + S.lim * d)))).attr("fill", "none").attr("stroke", K.orange).attr("stroke-width", 2.6);
      const cur = g.append("g");
      cur.append("line").attr("stroke", K.ink).attr("stroke-dasharray", "3 3");
      cur.append("circle").attr("r", 5.5).attr("fill", K.teal).attr("stroke", "#fff"); cur.append("circle").attr("class", "l").attr("r", 5.5).attr("fill", K.orange).attr("stroke", "#fff");
      const lab = cur.append("text").attr("class", "label").style("font-weight", 700).attr("fill", K.ink); halo(lab);
      const box = svg.append("rect").attr("x", px).attr("y", py).attr("width", pw).attr("height", ph).attr("fill", "none").attr("stroke", K.grid).attr("rx", 6).lower();
      return { S, g, cur, lab, outer: svg.append("g") , box, px, py };
    });
    function set(d) {
      panels.forEach(p => {
        const S = p.S, yp = y(pct(S.prof * d)), yl = y(pct(S.gap + S.lim * d)), gap = (1 - Math.exp(S.gap + (S.lim - S.prof) * d)) * 100;
        p.cur.attr("transform", `translate(${x(d)},0)`);
        p.cur.select("line").attr("y1", yp).attr("y2", yl); p.cur.select("circle").attr("cy", yp); p.cur.select("circle.l").attr("cy", yl);
        p.lab.attr("x", d > 1.2 ? -8 : 8).attr("text-anchor", d > 1.2 ? "end" : "start").attr("y", (yp + yl) / 2 + 4).text(`${gap.toFixed(0)}% gap`);
        p.gap = gap;
      });
    }
    function focus(idx) { panels.forEach((p, i) => { p.g.transition().duration(T).style("opacity", !idx || idx.includes(i) ? 1 : 0.28); }); }
    set(opts.d || 0);
    return { set, focus, panels, step(i) { focus(i === 0 ? [0] : i === 1 ? [0, 1] : null); } };
  };

  // ---------- chapter 9: own-language density ----------
  // opts: { width, height, pumas, O: results.ownlang, top: [{id, label, value}], sel: selection check rows }
  CH.ownLang = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, O = opts.O;
    const colW = Math.min(w * 0.32, 290), gm = svg.append("g"), mapH = h * 0.42;
    // map of the five PUMAs with the highest own-language values in the paper
    if (opts.pumas) {
      const G = opts.pumas, proj = d3.geoMercator().fitExtent([[0, 40], [colW, mapH]], G), path = d3.geoPath(proj);
      gm.append("text").attr("class", "title").attr("y", 14).text("Where Spanish is spoken at home");
      gm.append("text").attr("class", "note").attr("y", 30).attr("fill", K.muted).text("the five highest own-language values, all Spanish");
      const top = new Map(opts.top.map(t => [t.id, t]));
      const sh = gm.append("g").selectAll("path").data(G.features).join("path").attr("d", path).attr("fill", f => top.has(f.properties.STATE + f.properties.PUMA) ? K.orange : "#efefec").attr("fill-opacity", f => top.has(f.properties.STATE + f.properties.PUMA) ? 0.3 + 0.7 * (top.get(f.properties.STATE + f.properties.PUMA).value / 41) : 1).attr("stroke", "#fff").attr("stroke-width", 0.7);
      hover(sh, f => `<b>${f.properties.name.replace(" PUMA", "")}</b><br>${top.has(f.properties.STATE + f.properties.PUMA) ? top.get(f.properties.STATE + f.properties.PUMA).value + "% speak Spanish at home" : "not among the five highest values reported"}`);
      gm.append("path").datum({ type: "FeatureCollection", features: G.features.filter(f => f.properties.STATE === "PA" && String(f.properties.PUMA).startsWith("032")) }).attr("d", path).attr("fill", "none").attr("stroke", K.ink).attr("stroke-width", 1).attr("pointer-events", "none");
      const lg = gm.append("g").attr("transform", `translate(0,${mapH + 22})`);
      opts.top.forEach((t, i) => {
        const f = G.features.find(f => f.properties.STATE + f.properties.PUMA === t.id), c = path.centroid(f);
        lg.append("text").attr("class", "label").attr("x", 0).attr("y", i * 17).style("font-weight", 700).attr("fill", K.orange).text(`${t.value}%`);
        lg.append("text").attr("class", "note").attr("x", 36).attr("y", i * 17).attr("fill", K.ink).text(t.label);
        gm.append("text").attr("class", "note").attr("x", c[0]).attr("y", c[1] + 3).attr("text-anchor", "middle").style("font-size", "9px").style("font-weight", 700).attr("fill", "#fff").attr("pointer-events", "none").text(t.value);
      });
    }
    // selection check, bottom, full width
    const gs = svg.append("g").attr("transform", `translate(0,${h * 0.76})`).style("opacity", 0);
    (function () {
      const rows = opts.sel, m = { l: 190 }, cw = w - m.l - 200;
      gs.append("text").attr("class", "title").attr("y", 0).text("Do the proficient workers who leave a concentrated area earn more than those who stay?");
      gs.append("text").attr("class", "note").attr("y", 16).attr("fill", K.muted).text("4,370 proficient workers with a non-English home language who lived in the metro a year earlier · 98 had left their prior migration area · 95 percent intervals");
      const x = d3.scaleLinear().domain([-40, 40]).range([0, cw]), yb = d3.scaleBand().domain(rows.map(r => r.g)).range([28, 28 + rows.length * 30]).padding(0.4);
      const g = gs.append("g").attr("transform", `translate(${m.l},0)`);
      g.append("g").attr("class", "axis").attr("transform", `translate(0,${yb.range()[1] + 2})`).call(d3.axisBottom(x).ticks(5).tickFormat(d => fp(d) + "%").tickSizeOuter(0));
      g.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 26).attr("y2", yb.range()[1] + 2).attr("stroke", K.ink);
      const r = g.selectAll("g.r").data(rows).join("g").attr("class", "r").attr("transform", d => `translate(0,${yb(d.g) + yb.bandwidth() / 2})`);
      r.append("text").attr("class", "label").attr("x", -10).attr("dy", 4).attr("text-anchor", "end").text(d => d.g);
      r.append("line").attr("x1", d => x(Math.max(-40, d.v - 1.96 * d.se))).attr("x2", d => x(Math.min(40, d.v + 1.96 * d.se))).attr("stroke", K.grey).attr("stroke-width", 3).attr("stroke-linecap", "round");
      r.append("circle").attr("cx", d => x(d.v)).attr("r", 5.5).attr("fill", K.tealDark).attr("stroke", "#fff");
      r.append("text").attr("class", "note").attr("x", cw + 10).attr("dy", 4).attr("fill", K.ink2).text(d => `${fp(d.v)}% (SE ${d.se})`);
    })();
    // right: two slope panels, Philadelphia and New York
    const rx = colW + 40, rw = w - rx, panelsG = svg.append("g").attr("transform", `translate(${rx},0)`);
    const ph = (h * 0.72) / 2, m = { t: 72, r: 20, b: 40, l: 46 }, cw = rw - m.l - m.r, ch = ph - m.t - m.b;
    const y = d3.scaleLinear().domain([-16, 8]).range([ch, 0]);
    const cities = ["Philadelphia", "New York"], pts = d3.range(-1, 2.01, 0.05);
    const PN = cities.map((c, i) => {
      const o = O[c], g = panelsG.append("g").attr("transform", `translate(${m.l},${i * ph + m.t})`);
      const x = d3.scaleLinear().domain([-1, 2]).range([0, cw]);
      g.append("text").attr("class", "title").attr("y", -56).text(`${c}: wage by own-language density`);
      g.append("text").attr("class", "note").attr("y", -42).attr("fill", K.muted).text(`${d3.format(",")(o.n)} workers with a non-English home language · 1 SD = ${o.sd_pp} points · change from each group's level at the mean`);
      const nt = g.append("text").attr("class", "note").attr("y", -28);
      nt.append("tspan").attr("fill", K.tealDark).style("font-weight", 700).text(`proficient ${fp1(o.prof * 100)}% per SD (SE ${(o.prof_se * 100).toFixed(1)})`);
      nt.append("tspan").attr("fill", K.muted).text("   ");
      nt.append("tspan").attr("fill", K.orange).style("font-weight", 700).text(`limited English ${fp1(o.lim * 100)}% per SD (SE ${(o.lim_se * 100).toFixed(1)})`);
      g.append("text").attr("class", "note").attr("y", -14).attr("fill", K.ink2).text(`interaction ${f3(o.inter)} (SE ${o.inter_se}) · within occupation ${f3(o.inter_occ)} (SE ${o.inter_occ_se})`);
      g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(5).tickSize(-cw).tickFormat(""));
      g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(5).tickFormat(d => fp(d) + "%").tickSizeOuter(0)).select(".domain").remove();
      const xa = g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([-1, 0, 1, 2]).tickFormat(d => d === 0 ? "mean" : fp(d) + " SD").tickSizeOuter(0));
      xa.selectAll(".tick").append("text").attr("class", "note").attr("y", 27).attr("fill", K.muted).attr("text-anchor", "middle").text(d => (d === 0 ? "" : fp(d * o.sd_pp) + " points"));
      g.append("line").attr("x1", 0).attr("x2", cw).attr("y1", y(0)).attr("y2", y(0)).attr("stroke", K.axis);
      const draw = (b, se, col) => {
        g.append("path").datum(pts).attr("d", d3.area().x(d => x(d)).y0(d => y(pct(b * d - 1.96 * se * Math.abs(d)))).y1(d => y(pct(b * d + 1.96 * se * Math.abs(d))))).attr("fill", col).attr("fill-opacity", 0.12);
        g.append("path").datum(pts).attr("d", d3.line().x(d => x(d)).y(d => y(pct(b * d)))).attr("fill", "none").attr("stroke", col).attr("stroke-width", 2.8);
      };
      draw(o.lim, o.lim_se, K.orange); draw(o.prof, o.prof_se, K.teal);
      return g;
    });
    function step(i) {
      PN.forEach((g, k) => g.transition().duration(T).style("opacity", i === 0 ? 0.15 : i === 1 ? (k === 0 ? 1 : 0.15) : i === 3 ? 0.35 : 1));
      gm.transition().duration(T).style("opacity", i === 3 ? 0.35 : 1);
      gs.transition().duration(T).style("opacity", i === 3 ? 1 : 0);
    }
    return { step };
  };

  // ---------- chapter 10: Philadelphia against New York ----------
  // blocks: [{ title, domain, fmt, rows: [{ label, phl:[est,se], nyc:[est,se], note }] }]
  CH.cityCompare = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, B = opts.blocks;
    const labW = Math.min(230, w * 0.26), noteW = Math.min(250, w * 0.27), m = { l: labW, r: noteW + 16, t: 92 }, cw = w - m.l - m.r;
    const nRows = d3.sum(B, b => b.rows.length), rowH = Math.min(46, (h - m.t - B.length * 44) / nRows);
    svg.append("text").attr("class", "title").attr("y", 18).text("The same models in the two metropolitan areas, 2022–2024");
    svg.append("text").attr("class", "note").attr("y", 34).attr("fill", K.muted).text("dots are the estimates, bars the 95 percent intervals · New York is a comparison run with the same sample rules and specifications, not part of the design");
    // context strip: density of the two areas
    const ctx = svg.append("g").attr("transform", `translate(${m.l},52)`);
    const dx = d3.scaleLinear().domain([0, 66]).range([0, cw]);
    [["Philadelphia", K.teal, 12, 4, 28, "7,355 workers · 48 PUMAs"], ["New York", K.orange, 30, 4, 64, "68,058 workers · 141 PUMAs"]].forEach(([c, col, mu, lo, hi, n], i) => {
      const yy = i * 14;
      ctx.append("line").attr("x1", dx(lo)).attr("x2", dx(hi)).attr("y1", yy).attr("y2", yy).attr("stroke", col).attr("stroke-width", 6).attr("stroke-opacity", 0.35).attr("stroke-linecap", "round");
      ctx.append("circle").attr("cx", dx(mu)).attr("cy", yy).attr("r", 4).attr("fill", col);
      ctx.append("text").attr("class", "note").attr("x", -10).attr("y", yy + 4).attr("text-anchor", "end").attr("fill", col).style("font-weight", 700).text(c);
      ctx.append("text").attr("class", "note").attr("x", dx(hi) + 8).attr("y", yy + 4).attr("fill", K.ink2).text(`PUMAs ${lo} to ${hi} percent foreign-born, mean ${mu} · ${n}`);
    });
    let yy = m.t + 10;
    const items = [];
    B.forEach((b, bi) => {
      const g = svg.append("g").attr("transform", `translate(${m.l},${yy})`).attr("class", "blk");
      const x = d3.scaleLinear().domain(b.domain).range([0, cw]);
      g.append("text").attr("class", "label").style("font-weight", 700).attr("x", -labW + 0).attr("y", 0).text(b.title);
      const bh = b.rows.length * rowH;
      g.append("g").attr("class", "grid").attr("transform", "translate(0,10)").call(d3.axisBottom(x).ticks(6).tickSize(bh).tickFormat("")).select(".domain").remove();
      g.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 10).attr("y2", 10 + bh).attr("stroke", K.ink);
      g.append("g").attr("class", "axis").attr("transform", `translate(0,${10 + bh})`).call(d3.axisBottom(x).ticks(6).tickFormat(b.fmt).tickSizeOuter(0));
      b.rows.forEach((r, ri) => {
        const ry = 10 + ri * rowH + rowH / 2, rg = g.append("g").attr("transform", `translate(0,${ry})`);
        rg.append("text").attr("class", "label").attr("x", -12).attr("dy", 4).attr("text-anchor", "end").text(r.label);
        [["phl", K.teal, -6], ["nyc", K.orange, 6]].forEach(([k, col, off]) => {
          const [e, se] = r[k], t = Math.abs(e / se);
          const it = rg.append("g").attr("transform", `translate(0,${off})`).datum({ ...r, city: k === "phl" ? "Philadelphia" : "New York", e, se, t });
          it.append("line").attr("x1", x(e - 1.96 * se)).attr("x2", x(e + 1.96 * se)).attr("stroke", col).attr("stroke-width", 3).attr("stroke-linecap", "round");
          it.append("circle").attr("cx", x(e)).attr("r", 5).attr("fill", col).attr("stroke", "#fff").attr("stroke-width", 1.5);
          items.push({ sel: it, t });
          hover(it, d => `<b>${d.city} · ${d.label}</b><br>${f3(d.e)} (SE ${d.se.toFixed(3)}) · |t| = ${d.t.toFixed(1)}`);
        });
        halo(rg.append("text").attr("class", "note").attr("x", cw + 16).attr("dy", 4).attr("fill", K.ink2).text(r.note || ""));
      });
      yy += 10 + bh + 44; b.g = g;
    });
    function bonferroni(on) {
      items.forEach(o => { o.sel.select("circle").transition().duration(T).attr("stroke", on && o.t > 2.92 ? K.ink : "#fff").attr("stroke-width", on && o.t > 2.92 ? 3 : 1.5); o.sel.transition().duration(T).style("opacity", on && o.t <= 2.92 ? 0.3 : 1); });
    }
    function focus(idx) { B.forEach((b, i) => b.g.transition().duration(T).style("opacity", idx == null || idx.includes(i) ? 1 : 0.3)); }
    return { bonferroni, focus, step(i) { focus(i === 0 ? [0, 1] : i === 1 ? [2] : [3]); bonferroni(i === 2); } };
  };
})();
