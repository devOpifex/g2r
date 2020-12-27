import 'widgets';
import { Chart } from '@antv/g2';
import { makeFigure } from '../modules/makeFigure.js'; 
import { makeCoords } from '../modules/makeCoords.js'; 
import { tuneFigure } from '../modules/tuneFigure.js';
import { assignData } from '../modules/assignData.js';

HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    let c;

    return {

      renderValue: function(x) {

        // keep autofit for responsiveness
        c = new Chart({
          container: el.id,
          autoFit: true,
        });

        if(x.data)
          c.data(x.data);

        // scale
        if(x.scale)
          c.scale(x.scale);

        // Coordinates
        makeCoords(c, x);

        if(x.axis){
          x.axis.forEach(function(ax){
            c.axis(ax.column, ax.opts);
          })
        }

        if(x.legend)
          x.legend.forEach(function(l){
            c.legend(l.column, l.opts);
          })

        // loop over figures
        if(x.facet){

          // data
          c.data(x.data);

          // views
          x.facet.opts.eachView = makeFacetView(x)

          c.facet(x.facet.type, x.facet.opts)

        } else {
          x.views.forEach(function(v){
            let view = c.createView();
  
            let figure = makeFigure(view, v.type);
            tuneFigure(figure, v);
  
            // data
            assignData(view, v, x)
            
          });
        }

        c.render();

      },

      resize: function(width, height) {

      },
      getC: function(){
        return c;
      }

    };
  }
});

function makeFacetView(x){
  return function(view){
    x.views.forEach(function(v){

      let figure = makeFigure(view, v.type);
      tuneFigure(figure, v);
      
    });
  }
}
