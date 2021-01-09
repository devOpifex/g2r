import { gaugeFigure } from './gauges.js';
import { addFigure } from './figures.js'; 

const facetFactory = (views) =>   {
  return function(view, facet){
    view.data(facet.data);
    
    views.forEach(function(v){

      let figure = addFigure(view, v.type);
      gaugeFigure(figure, v);
      
    });
  }
}

export { facetFactory }
