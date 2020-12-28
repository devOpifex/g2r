import 'widgets';
import { Chart } from '@antv/g2';
import { makeFigure } from '../modules/makeFigure.js'; 
import { makeCoords } from '../modules/makeCoords.js'; 
import { tuneFigure } from '../modules/tuneFigure.js';
import { assignData } from '../modules/assignData.js';
import { annotate } from '../modules/annotate.js';
import { captureEvents } from '../modules/events.js';
import { interactions } from '../modules/interactions.js';

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

        // interactions
        interactions(c, x);

        // events
        captureEvents(c, x);

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
          x.facet.opts.eachView = makeFacetView(x)

          c.facet(x.facet.type, x.facet.opts)

        } else {
          x.views.forEach(function(v){
            let view = c.createView();

            annotate(view, x)
  
            let figure = makeFigure(view, v.type);
            tuneFigure(figure, v);
  
            // data
            assignData(view, v, x)
            
          });
        }

        if(x.slider)
          c.option('slider', x.slider);

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
