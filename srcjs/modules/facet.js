import { tuneFigure } from './tuneFigure.js';
import { makeFigure } from './makeFigure.js'; 

const facetFactory = (x) =>   {
  return function(view, facet){
    view.data(facet.data);
    
    x.views.forEach(function(v){

      let figure = makeFigure(view, v.type);
      tuneFigure(figure, v);
      
    });
  }
}

export { facetFactory }
