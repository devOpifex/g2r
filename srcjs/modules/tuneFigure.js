const check = (options, name) => {
  typeof options[name] == 'object' && options[name].length == 2
}

const tuneFigure = (figure, options) => {

  // position
  figure.position(options.position);

  // adjust
  if(options.adjust)
    figure.adjust(options.adjust);

  // color
  if(options.color)
    if(check(color))
      figure.color(options.color[0], options.color[1]);
    else 
      figure.color(options.color);

  // shape
  if(options.shape)
    if(check(shape))
      figure.shape(options.shape[0], options.shape[1]);
    else 
      figure.shape(options.shape);

  // size
  if(options.size)
    if(check(size))
      figure.size(options.size[0], options.size[1]);
    else 
      figure.size(options.size);

  // size
  if(options.label)
    if(check(label))
      figure.label(options.label[0], options.label[1]);
    else 
      figure.size(options.label);

  // tooltip
  if(options.tooltip)
    figure.tooltip(...options.tooltip[0]);
}

export { tuneFigure };
