import { check } from './utils.js'

const globalInteractions = (c, options) => {

  if(!options.interactions)
    return ;

  options.interactions.forEach(inter => {
    c.interaction(inter)
  })
}

const interactions = (v, options) => {

  if(!options.interaction)
    return ;

  if(check(options, "interaction"))
    v.interaction(options.interaction[0], options.interaction[1]);
  else
    v.interaction(options.interaction);
}

const rmInteractions = (c, options) => {
  if(!options.rmInteractions)
    return ;

  options.rmInteractions.forEach(inter => {
    c.removeInteraction(inter)
  });
}

export { globalInteractions, interactions, rmInteractions };
