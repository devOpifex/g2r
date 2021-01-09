const makeCoords = (c, coordOpts, coordRotate, coordScale, coordReflect, coordTranspose) => {
  let coord;

  if(coordOpts !== undefined)
    coord = c.coordinate(coordOpts.type, coordOpts.opts);

  if(coordRotate !== undefined)
    coord.rotate(coordRotate);

  if(coordScale !== undefined)
    coord.scale(coordScale[0], coordScale[1]);

  if(coordReflect !== undefined)
    coord.reflect(coordReflect);

  if(coordTranspose !== undefined)
    coord.transpose();
}

export { makeCoords };
