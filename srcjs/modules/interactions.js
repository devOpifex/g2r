const interactions = (c, options) => {

  if(!options.interactions)
    return ;

  options.interactions.forEach(function(inter){
    c.interaction(inter);
  })
}

export { interactions };
