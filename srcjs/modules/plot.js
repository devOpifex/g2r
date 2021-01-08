import { makeFigure } from './makeFigure.js'; 
import { makeCoords } from './makeCoords.js'; 
import { tuneFigure } from './tuneFigure.js';
import { getData } from './assignData.js';
import { annotate } from './annotate.js';
import { captureEvents } from './events.js';
import { 
  globalInteractions, 
  interactions, 
  rmInteractions
} from './interactions.js';
import { facetFactory } from './facet.js';
import { getView } from './shiny.js';

const plot = (c, x, el) => {
  // scale
  if(x.scale)
    c.scale(x.scale);

  // Coordinates
  makeCoords(c, x);

  // interactions
  rmInteractions(c, x);
  globalInteractions(c, x);

  // events
  if(el !== undefined)
    captureEvents(c, el, x);

  if(x.axis){
    x.axis.forEach(function(ax){
      c.axis(ax.column, ax.opts);
    })
  }

  if(x.legend)
    x.legend.forEach(function(l){
      c.legend(l.column, l.opts);
    })

  if(x.tooltip)
    c.tooltip(x.tooltip);

  if(x.style)
    c.style(x.style);

  // loop over figures
  if(x.facet){

    // data
    c.data(x.data);

    // views
    x.facet.opts.eachView = facetFactory(x);

    c.facet(x.facet.type, x.facet.opts);

  } else {
    x.views.forEach(function(layer){
      let view;
        
      // retrieve view instead of create if id is passed
      if(layer.conf && layer.conf.id)
        view = getView(x.id, layer.conf.id);

      let existing = true;
      let data = getData(x.data, layer.data);

      // if not found create one
      if(view === undefined)
        view = c.createView(layer.conf);
      else 
        existing = false;

      annotate(view, layer);

      interactions(view, layer);

      let figure = makeFigure(view, layer.type);
      tuneFigure(figure, layer);

      if(existing){
        view.changeData(data);
      } else {
        view.data(data);
      }
      
    });
  }

  if(x.slider)
    c.option('slider', x.slider);

  if(x.scrollbar)
    c.option('scrollbar', x.scrollbar);
}

export { plot }
