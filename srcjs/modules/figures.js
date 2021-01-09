const addFigure = (view, type) => {
  let e;

  // view type
  if(type == "point")
    e = view.point();

  if(type == "interval")
    e = view.interval();

  if(type == "line")
    e = view.line();

  if(type == "area")
    e = view.area();

  if(type == "polygon")
    e = view.polygon();

  if(type == "schema")
    e = view.schema();

  if(type == "edge")
    e = view.edge();

  if(type == "path")
    e = view.path();

  if(type == "heatmap")
    e = view.heatmap();

  return e;
}

export { addFigure };
