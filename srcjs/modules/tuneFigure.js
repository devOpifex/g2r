const tuneFigure = (figure, options) => {

  // position
  figure.position(options.position);

  // adjust
  if(options.adjust)
    figure.adjust(options.adjust);

  // color
  if(options.color)
    figure.color(options.color);

  // shape
  if(options.shape)
    figure.shape(options.shape);

  // size
  if(options.size)
    figure.size(options.size);

  // tooltip
  if(options.tooltip)
    figure.tooltip(options.tooltip[0].aspects, options.tooltip[0].callback);
}

export { tuneFigure };
