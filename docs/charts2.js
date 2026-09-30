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
    const w = opts.width, h = opts.height, O = opts.O, o = O.Philadelphia;
    const colW = Math.round(w * 0.55), gm = svg.append("g"), mapH = h - 130;
    const G = opts.pumas, proj = d3.geoMercator().fitExtent([[0, 44], [colW, mapH]], G), path = d3.geoPath(proj);
    const top = new Map(opts.top.map(t => [t.id, t]));
    const L = opts.lang; // optional: rows {STATE, PUMA, language, pct} from data/language_puma.csv
    const langs = L ? [...new Set(L.map(d => d.language))] : [];
    const ttl = gm.append("text").attr("class", "title").attr("y", 16), sub = gm.append("text").attr("class", "note").attr("y", 32).attr("fill", K.muted);
    const shapes = gm.append("g").selectAll("path").data(G.features).join("path").attr("d", path).attr("stroke", "#fff").attr("stroke-width", 0.8).style("cursor", "pointer");
    gm.append("path").datum({ type: "FeatureCollection", features: G.features.filter(f => f.properties.STATE === "PA" && String(f.properties.PUMA).startsWith("032")) }).attr("d", path).attr("fill", "none").attr("stroke", K.ink).attr("stroke-width", 1.1).attr("pointer-events", "none");
    const labG = gm.append("g").attr("pointer-events", "none"), lg = gm.append("g").attr("transform", `translate(0,${mapH + 24})`);
    function paperView() {
      ttl.text("Where Spanish is spoken at home"); sub.text("the five highest own-language values in the paper, all Spanish · percent of residents aged 5 and over");
      shapes.attr("fill", f => top.has(f.properties.STATE + f.properties.PUMA) ? K.orange : "#efefec").attr("fill-opacity", f => top.has(f.properties.STATE + f.properties.PUMA) ? 0.3 + 0.7 * (top.get(f.properties.STATE + f.properties.PUMA).value / 41) : 1);
      hover(shapes, f => `<b>${f.properties.name.replace(" PUMA", "")}</b><br>${top.has(f.properties.STATE + f.properties.PUMA) ? top.get(f.properties.STATE + f.properties.PUMA).value + "% speak Spanish at home" : "not among the five highest values the paper reports"}`);
      labG.selectAll("*").remove(); lg.selectAll("*").remove();
      opts.top.forEach((t, i) => {
        const f = G.features.find(f => f.properties.STATE + f.properties.PUMA === t.id), c = path.centroid(f);
        labG.append("text").attr("class", "note").attr("x", c[0]).attr("y", c[1] + 4).attr("text-anchor", "middle").style("font-size", "11px").style("font-weight", 700).attr("fill", "#fff").text(t.value + "%");
        const col = i < 3 ? 0 : 1, row = i % 3, cx0 = col * Math.max(250, colW / 2);
        lg.append("text").attr("class", "label").attr("x", cx0).attr("y", row * 18).style("font-weight", 700).attr("fill", K.orange).text(`${t.value}%`);
        lg.append("text").attr("class", "note").attr("x", cx0 + 38).attr("y", row * 18).attr("fill", K.ink).text(t.label);
      });
    }
    function langView(lang) {
      const rows = L.filter(d => d.language === lang), by = new Map(rows.map(d => [d.STATE + d.PUMA, +d.pct])), ext = [0, d3.max(rows, d => +d.pct)];
      const sc = d3.scaleSequential(d3.interpolate("#fbe4cf", "#9a4d00")).domain(ext);
      ttl.text(`Residents who speak ${lang} at home, by PUMA`); sub.text("percent of residents aged 5 and over, 2022–2024 · the own-language measure counts native-born speakers too");
      shapes.attr("fill", f => by.has(f.properties.STATE + f.properties.PUMA) ? sc(by.get(f.properties.STATE + f.properties.PUMA)) : "#efefec").attr("fill-opacity", 1);
      hover(shapes, f => `<b>${f.properties.name.replace(" PUMA", "")}</b><br>${by.has(f.properties.STATE + f.properties.PUMA) ? by.get(f.properties.STATE + f.properties.PUMA).toFixed(1) + "% speak " + lang + " at home" : "no value"}`);
      labG.selectAll("*").remove(); lg.selectAll("*").remove();
      const ls = d3.scaleLinear().domain(ext).range([0, 140]);
      d3.range(0, 141, 3).forEach(v => lg.append("rect").attr("x", v).attr("y", 0).attr("width", 3).attr("height", 8).attr("fill", sc(ls.invert(v))));
      lg.append("text").attr("class", "note").attr("y", 22).text("0%"); lg.append("text").attr("class", "note").attr("x", 140).attr("y", 22).attr("text-anchor", "end").text(ext[1].toFixed(0) + "%");
    }
    paperView();
    // right column: Philadelphia slopes (top) and the selection check (bottom)
    const rx = colW + 50, rw = w - rx, m = { t: 76, r: 20, b: 44, l: 46 }, cw = rw - m.l - m.r;
    const ph = h * 0.6, ch = ph - m.t - m.b, g = svg.append("g").attr("transform", `translate(${rx + m.l},${m.t})`);
    const x = d3.scaleLinear().domain([-1, 2]).range([0, cw]), y = d3.scaleLinear().domain([-16, 8]).range([ch, 0]), pts = d3.range(-1, 2.01, 0.05);
    g.append("text").attr("class", "title").attr("y", -58).text("Wage by own-language density, Philadelphia");
    g.append("text").attr("class", "note").attr("y", -44).attr("fill", K.muted).text(`${d3.format(",")(o.n)} workers with a non-English home language · 1 SD = ${o.sd_pp} points`);
    g.append("text").attr("class", "note").attr("y", -30).attr("fill", K.muted).text("change from the group's level at the mean · bands: 95% slope interval");
    const nt = g.append("text").attr("class", "note").attr("y", -14);
    nt.append("tspan").attr("fill", K.tealDark).style("font-weight", 700).text(`proficient ${fp1(o.prof * 100)}% per SD (SE ${(o.prof_se * 100).toFixed(1)})`);
    nt.append("tspan").attr("fill", K.muted).text("   ");
    nt.append("tspan").attr("fill", K.orange).style("font-weight", 700).text(`limited English ${fp1(o.lim * 100)}% per SD (SE ${(o.lim_se * 100).toFixed(1)})`);
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
    halo(g.append("text").attr("class", "note").attr("x", 4).attr("y", 14).attr("fill", K.ink2).text(`interaction ${f3(o.inter)} (SE ${o.inter_se}) · within occupation ${f3(o.inter_occ)} (SE ${o.inter_occ_se})`));
    // the selection check
    const gs = svg.append("g").attr("transform", `translate(${rx},${ph + 40})`).style("opacity", 0);
    (function () {
      const rows = opts.sel, ml = 150, cws = rw - ml - 110;
      gs.append("text").attr("class", "title").attr("y", 0).text("Do proficient leavers earn more than stayers?");
      gs.append("text").attr("class", "note").attr("y", 16).attr("fill", K.muted).text("4,370 proficient workers with a non-English home language");
      gs.append("text").attr("class", "note").attr("y", 30).attr("fill", K.muted).text("98 had left their prior migration area within the year · 95 percent intervals");
      const xs = d3.scaleLinear().domain([-40, 40]).range([0, cws]), yb = d3.scaleBand().domain(rows.map(r => r.g)).range([44, 44 + rows.length * 32]).padding(0.4);
      const gg = gs.append("g").attr("transform", `translate(${ml},0)`);
      gg.append("g").attr("class", "axis").attr("transform", `translate(0,${yb.range()[1] + 2})`).call(d3.axisBottom(xs).ticks(5).tickFormat(d => fp(d) + "%").tickSizeOuter(0));
      gg.append("line").attr("x1", xs(0)).attr("x2", xs(0)).attr("y1", 42).attr("y2", yb.range()[1] + 2).attr("stroke", K.ink);
      const r = gg.selectAll("g.r").data(rows).join("g").attr("class", "r").attr("transform", d => `translate(0,${yb(d.g) + yb.bandwidth() / 2})`);
      r.append("text").attr("class", "label").attr("x", -10).attr("dy", 4).attr("text-anchor", "end").text(d => d.g);
      r.append("line").attr("x1", d => xs(Math.max(-40, d.v - 1.96 * d.se))).attr("x2", d => xs(Math.min(40, d.v + 1.96 * d.se))).attr("stroke", K.grey).attr("stroke-width", 3).attr("stroke-linecap", "round");
      r.append("circle").attr("cx", d => xs(d.v)).attr("r", 5.5).attr("fill", K.tealDark).attr("stroke", "#fff");
      r.append("text").attr("class", "note").attr("x", cws + 8).attr("dy", 4).attr("fill", K.ink2).text(d => `${fp(d.v)}% (SE ${d.se})`);
    })();
    function step(i) {
      g.transition().duration(T).style("opacity", i === 0 ? 0.15 : i === 2 ? 0.35 : 1);
      gm.transition().duration(T).style("opacity", i === 2 ? 0.35 : 1);
      gs.transition().duration(T).style("opacity", i === 2 ? 1 : 0);
    }
    return { step, langs, setLang: l => (l === "paper" || !L ? paperView() : langView(l)) };
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
    return { bonferroni, focus, step(i) { focus(i === 0 ? [0, 1] : i === 1 ? [2] : i === 2 ? [3] : [4]); bonferroni(i === 2); } };
  };

  // ---------- chapter 11: cohorts over twelve years ----------
  // opts: { width, height, cells, tab7 (Table 5 rows), fe: {b, se}, regionColor }
  CH.cohortPanel = function (svg, opts) {
    svg.selectAll("*").remove();
    const w = opts.width, h = opts.height, RC = opts.regionColor;
    const cells = opts.cells.map(d => ({ ...d, suburb_prop: +d.suburb_prop, median_hourly: +d.median_hourly, n: +d.n_unweighted }));
    const cn = { pre1990: "before 1990", "1990s": "1990s", "2000s": "2000s", "2010s": "2010s" }, wn = s => s.replace("-", "–");
    const sideW = Math.min(330, w * 0.38), m = { t: 54, r: sideW + 40, b: 56, l: 56 }, cw = w - m.l - m.r, ch = h - m.t - m.b;
    const g = svg.append("g").attr("transform", `translate(${m.l},${m.t})`);
    const x = d3.scaleLog().domain([14, 46]).range([0, cw]), y = d3.scaleLinear().domain([50, 90]).range([ch, 0]);
    g.append("text").attr("class", "title").attr("y", -34).text("Sixteen cohort-by-region cells across three windows");
    g.append("text").attr("class", "note").attr("y", -18).attr("fill", K.muted).text("2012–2016 → 2017–2021 → 2022–2024 · hollow start, filled end · thick: 2010s arrivals · hover a path or a row");
    g.append("g").attr("class", "grid").call(d3.axisLeft(y).ticks(5).tickSize(-cw).tickFormat(""));
    g.append("g").attr("class", "axis").call(d3.axisLeft(y).ticks(5).tickFormat(d => d + "%").tickSizeOuter(0)).select(".domain").remove();
    g.append("g").attr("class", "axis").attr("transform", `translate(0,${ch})`).call(d3.axisBottom(x).tickValues([15, 20, 25, 30, 35, 40, 45]).tickFormat(d => "$" + d).tickSizeOuter(0));
    g.append("text").attr("class", "label").attr("x", cw / 2).attr("y", ch + 40).attr("text-anchor", "middle").text("median hourly wage, 2024 dollars (log scale)");
    g.append("text").attr("class", "label").attr("transform", `translate(-42,${ch / 2}) rotate(-90)`).attr("text-anchor", "middle").text("percent living outside Philadelphia County");
    // cell fixed-effects slope, drawn through the population-weighted mean for scale
    const mx = d3.sum(cells, d => d.pop * Math.log(d.median_hourly)) / d3.sum(cells, d => d.pop), my = d3.sum(cells, d => d.pop * d.suburb_prop) / d3.sum(cells, d => d.pop);
    const fe = opts.fe, feG = g.append("g");
    const lx = [Math.log(18), Math.log(42)];
    feG.append("path").datum(lx).attr("d", d3.area().x(d => x(Math.exp(d))).y0(d => y(my + (fe.b - 1.96 * fe.se) * (d - mx))).y1(d => y(my + (fe.b + 1.96 * fe.se) * (d - mx)))).attr("fill", K.grey).attr("fill-opacity", 0.07);
    feG.append("line").attr("x1", x(18)).attr("x2", x(42)).attr("y1", y(my + fe.b * (lx[0] - mx))).attr("y2", y(my + fe.b * (lx[1] - mx))).attr("stroke", K.ink2).attr("stroke-width", 1.2).attr("stroke-dasharray", "5 4");
    halo(feG.append("text").attr("class", "note").attr("x", 4).attr("y", ch - 20).attr("text-anchor", "start").attr("fill", K.ink2).text(`dashed: cell fixed-effects slope, ${fe.b} points per log point of wage (SE ${fe.se})`));
    halo(feG.append("text").attr("class", "note").attr("x", 4).attr("y", ch - 6).attr("text-anchor", "start").attr("fill", K.muted).text("grey: its 95 percent interval, about −11 to +31 · drawn through the weighted mean for scale"));
    // paths
    const byCell = d3.groups(cells, d => d.cell).map(([k, v]) => ({ k, region: v[0].region, cohort: v[0].cohort, pts: v.sort((a, b) => a.window.localeCompare(b.window)) }));
    const line = d3.line().x(d => x(d.median_hourly)).y(d => y(d.suburb_prop)).curve(d3.curveCatmullRom.alpha(0.5));
    const pathG = g.append("g"), ptG = g.append("g");
    const paths = pathG.selectAll("path").data(byCell).join("path").attr("fill", "none").attr("stroke", d => RC[d.region]).attr("stroke-width", d => d.cohort === "2010s" ? 3 : 1.6).attr("stroke-opacity", 0.85).attr("d", d => line(d.pts)).style("cursor", "pointer");
    const pts = ptG.selectAll("circle").data(cells).join("circle").attr("cx", d => x(d.median_hourly)).attr("cy", d => y(d.suburb_prop)).attr("r", d => d.window === "2012-2016" || (d.cohort === "2010s" && d.window === "2017-2021") ? 4 : d.window === "2022-2024" ? 4.5 : 2.5)
      .attr("fill", d => d.window === "2022-2024" ? RC[d.region] : "#fff").attr("stroke", d => RC[d.region]).attr("stroke-width", 1.6).style("cursor", "pointer");
    const tipTxt = d => `<b>${d.region}, arrived ${cn[d.cohort]}</b><br>${cells.filter(c => c.cell === d.cell).sort((a, b) => a.window.localeCompare(b.window)).map(p => `${wn(p.window)}: ${d3.format("$.0f")(p.median_hourly)}, ${p.suburb_prop.toFixed(0)}% outside the city, n = ${d3.format(",")(p.n)}`).join("<br>")}`;
    hover(paths, d => tipTxt(d.pts[0])); hover(pts, tipTxt);
    function focusCell(k) {
      paths.transition().duration(200).attr("stroke-opacity", d => !k || d.k === k ? (k ? 1 : 0.85) : 0.1);
      pts.transition().duration(200).style("opacity", d => !k || d.cell === k ? 1 : 0.1);
    }
    paths.on("mouseenter", (ev, d) => { focusCell(d.k); rowFocus(d.region); }).on("mouseleave", () => { focusCell(null); rowFocus(null); });
    pts.on("mouseenter", (ev, d) => { focusCell(d.cell); rowFocus(d.region); }).on("mouseleave", () => { focusCell(null); rowFocus(null); });
    // legend
    const leg = g.append("g").attr("transform", `translate(8,8)`);
    Object.entries(RC).forEach(([k, c], i) => { leg.append("line").attr("x1", 0).attr("x2", 16).attr("y1", i * 16).attr("y2", i * 16).attr("stroke", c).attr("stroke-width", 2.5); leg.append("text").attr("class", "note").attr("x", 22).attr("y", i * 16 + 4).text(k); });
    // right side: duration-matched comparison, two blocks from Table 5
    const side = svg.append("g").attr("transform", `translate(${m.l + cw + 40},${m.t})`);
    const parse = c => { const a = c.match(/([−-]?[\d.]+) \(([−-]?[\d.]+) to ([−-]?[\d.]+)\)/); return a ? { v: +a[1].replace("−", "-"), lo: +a[2].replace("−", "-"), hi: +a[3].replace("−", "-") } : null; };
    const pd = c => { const a = c.match(/([−-]?[\d.]+) \(([\d.]+)\)/); return { d: +a[1].replace("−", "-"), se: +a[2] }; };
    const blocks = [
      { title: "2010s arrivals against 2000s arrivals at matched duration", sub: "2000s cohort in 2012–2016 (hollow) vs 2010s cohort in 2022–2024 (filled)", a: [1, "2000s", "2012-2016"], b: [2, "2010s", "2022-2024"], di: 3 },
      { title: "One step earlier: 2000s against 1990s", sub: "1990s cohort in 2012–2016 (hollow) vs 2000s cohort in 2022–2024 (filled)", a: [4, "1990s", "2012-2016"], b: [5, "2000s", "2022-2024"], di: 6 }
    ];
    const xs = d3.scaleLinear().domain([44, 82]).range([0, sideW - 100]);
    const rowsAll = [];
    blocks.forEach((b, bi) => {
      const bg = side.append("g").attr("transform", `translate(0,${bi * (ch / 2 + 10)})`); b.g = bg;
      bg.append("text").attr("class", "label").style("font-weight", 700).attr("y", 0).text(b.title);
      bg.append("text").attr("class", "note").attr("y", 15).attr("fill", K.muted).text(b.sub);
      const rows = opts.tab7.map(r => ({ region: r[0], a: parse(r[b.a[0]]), b: parse(r[b.b[0]]), diff: pd(r[b.di]), ca: b.a, cb: b.b }));
      const yb = d3.scaleBand().domain(rows.map(r => r.region)).range([28, 28 + rows.length * 34]).padding(0.35);
      bg.append("g").attr("class", "axis").attr("transform", `translate(0,${yb.range()[1] + 4})`).call(d3.axisBottom(xs).ticks(5).tickFormat(d => d + "%").tickSizeOuter(0));
      const rg = bg.selectAll("g.r").data(rows).join("g").attr("class", "r").attr("transform", d => `translate(0,${yb(d.region) + yb.bandwidth() / 2})`).style("cursor", "pointer");
      rg.append("text").attr("class", "note").attr("x", 0).attr("y", -9).attr("fill", d => RC[d.region]).style("font-weight", 700).text(d => d.region);
      rg.append("line").attr("x1", d => xs(d.a.lo)).attr("x2", d => xs(d.a.hi)).attr("stroke", K.grey2).attr("stroke-width", 2);
      rg.append("line").attr("x1", d => xs(d.b.lo)).attr("x2", d => xs(d.b.hi)).attr("y1", 4).attr("y2", 4).attr("stroke", d => RC[d.region]).attr("stroke-width", 2).attr("stroke-opacity", 0.5);
      rg.append("line").attr("x1", d => xs(d.a.v)).attr("x2", d => xs(d.b.v)).attr("y1", 2).attr("y2", 2).attr("stroke", d => RC[d.region]).attr("stroke-width", 3);
      rg.append("circle").attr("cx", d => xs(d.a.v)).attr("r", 5).attr("fill", "#fff").attr("stroke", d => RC[d.region]).attr("stroke-width", 2);
      rg.append("circle").attr("cx", d => xs(d.b.v)).attr("cy", 4).attr("r", 5).attr("fill", d => RC[d.region]).attr("stroke", "#fff");
      halo(rg.append("text").attr("class", "note").attr("x", d => xs(Math.max(d.a.hi, d.b.hi)) + 6).attr("y", 5).style("font-weight", 700).attr("fill", d => Math.abs(d.diff.d / d.diff.se) >= 1.96 ? K.orange : K.ink2).text(d => `${d.diff.d > 0 ? "+" : ""}${d.diff.d} (SE ${d.diff.se})`));
      hover(rg, d => `<b>${d.region}</b><br>${cn[d.ca[1]]} cohort in ${wn(d.ca[2])}: ${d.a.v}% outside the city (${d.a.lo} to ${d.a.hi})<br>${cn[d.cb[1]]} cohort in ${wn(d.cb[2])}: ${d.b.v}% (${d.b.lo} to ${d.b.hi})<br>difference ${d.diff.d > 0 ? "+" : ""}${d.diff.d} points, SE ${d.diff.se}`);
      rg.on("mouseenter", (ev, d) => pairFocus(d)).on("mouseleave", () => pairFocus(null));
      rows.forEach(r => { r.sel = rg.filter(q => q === r); rowsAll.push(r); });
    });
    const link = g.append("g").attr("pointer-events", "none");
    function pairFocus(d) {
      link.selectAll("*").remove();
      if (!d) { focusCell(null); rowFocus(null); return; }
      const A = cells.find(c => c.region === d.region && c.cohort === d.ca[1] && c.window === d.ca[2]), B = cells.find(c => c.region === d.region && c.cohort === d.cb[1] && c.window === d.cb[2]);
      paths.transition().duration(150).attr("stroke-opacity", 0.08); pts.transition().duration(150).style("opacity", c => c === A || c === B ? 1 : 0.08);
      if (A && B) { link.append("line").attr("x1", x(A.median_hourly)).attr("y1", y(A.suburb_prop)).attr("x2", x(B.median_hourly)).attr("y2", y(B.suburb_prop)).attr("stroke", RC[d.region]).attr("stroke-width", 2.5).attr("stroke-dasharray", "4 3");
        [[A, cn[d.ca[1]] + " in " + wn(d.ca[2])], [B, cn[d.cb[1]] + " in " + wn(d.cb[2])]].forEach(([c, t]) => halo(link.append("text").attr("class", "note").attr("x", x(c.median_hourly) + 8).attr("y", y(c.suburb_prop) - 8).style("font-weight", 700).attr("fill", RC[d.region]).text(t))); }
    }
    function rowFocus(region) { rowsAll.forEach(r => r.sel.transition().duration(150).style("opacity", !region || r.region === region ? 1 : 0.25)); }
    function step(i) {
      blocks.forEach((b, bi) => b.g.transition().duration(T).style("opacity", i === 0 ? 0.25 : (i === 1 ? bi === 0 : bi === 1) ? 1 : 0.25));
      feG.transition().duration(T).style("opacity", i === 0 ? 1 : 0.25);
      link.selectAll("*").remove();
      if (i === 0) { paths.transition().duration(T).attr("stroke-opacity", 0.85); pts.transition().duration(T).style("opacity", 1); }
      else { const ca = i === 1 ? ["2000s", "2012-2016"] : ["1990s", "2012-2016"], cb = i === 1 ? ["2010s", "2022-2024"] : ["2000s", "2022-2024"];
        const keep = c => (c.cohort === ca[0] && c.window === ca[1]) || (c.cohort === cb[0] && c.window === cb[1]);
        paths.transition().duration(T).attr("stroke-opacity", d => d.cohort === cb[0] || d.cohort === ca[0] ? 0.5 : 0.08); pts.transition().duration(T).style("opacity", c => keep(c) ? 1 : 0.1);
        Object.keys(RC).forEach(rg => { const A = cells.find(c => c.region === rg && c.cohort === ca[0] && c.window === ca[1]), B = cells.find(c => c.region === rg && c.cohort === cb[0] && c.window === cb[1]);
          if (A && B) link.append("line").attr("x1", x(A.median_hourly)).attr("y1", y(A.suburb_prop)).attr("x2", x(B.median_hourly)).attr("y2", y(B.suburb_prop)).attr("stroke", RC[rg]).attr("stroke-width", 2).attr("stroke-dasharray", "4 3"); }); }
    }
    return { step };
  };
})();
