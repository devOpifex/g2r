import { gaugeFigure } from './gauges.js';
import { addFigure } from './figures.js'; 
import { getData } from './data.js'

const facetFactory = (views) =>   {
  return function(view, facet){
    view.data(facet.data);
    
    views.forEach(function(v){

      if(v.data){
        facet.data = v.data.filter(function(row){
          return row[facet.columnField] == facet.columnValue;
        }) 
      }

      view.data(facet.data);

      let figure = addFigure(view, v.type);
      gaugeFigure(figure, v);
      
    });
  }
}

export { facetFactory }
