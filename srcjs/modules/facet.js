import { tuneFigure } from './tuneFigure.js';
import { makeFigure } from './makeFigure.js'; 

const facetFactory = (x) =>   {
  return function(view){
    x.views.forEach(function(v){

      let figure = makeFigure(view, v.type);
      tuneFigure(figure, v);
      
    });
  }
}

export { facetFactory }
