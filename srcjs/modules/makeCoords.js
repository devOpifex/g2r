const makeCoords = (c, x) => {
  let coord;

  if(x.coord)
    coord = c.coordinate(x.coord.type, x.coord.opts);

  if(x.coordRotate)
    coord.rotate(x.coordRotate);

  if(x.coordScale)
    coord.scale(x.coordScale[0], x.coordScale[1]);

  if(x.coordReflect)
    coord.reflect(x.coordReflect);

  if(x.coordTranspose)
    coord.transpose();
}

export { makeCoords };
