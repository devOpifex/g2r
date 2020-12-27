const annotate = (view, options) => {

  if(!options.annotations)
    return ;

  options.annotations.forEach(function(info){
    view.annotation().text(info);
  })
}

export { annotate };
