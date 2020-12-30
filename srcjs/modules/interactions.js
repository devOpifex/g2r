import { check } from './utils.js'
import { registerInteraction } from '@antv/g2';

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

const registerInteractions = (options) => {
  if(!options.registerInteractions)
    return ;

  options.registerInteractions.forEach((reg) => {
    registerInteraction(reg.name, reg.opts);
  })
    
}

const rmInteractions = (c, options) => {
  if(!options.rmInteractions)
    return ;

  options.rmInteractions.forEach(inter => {
    c.removeInteraction(inter)
  });
}

export { globalInteractions, interactions, rmInteractions, registerInteractions };
