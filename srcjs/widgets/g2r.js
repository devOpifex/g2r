import 'widgets';
import { Chart } from '@antv/g2';
import { makeFigure } from '../modules/makeFigure.js'; 
import { makeCoords } from '../modules/makeCoords.js'; 
import { tuneFigure } from '../modules/tuneFigure.js';

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

        // loop over figures
        x.views.forEach(function(v){
          let view = c.createView();

          let figure = makeFigure(view, v.type);
          tuneFigure(figure, v);

          // data
          if(v.data)
            view.data(v.data);
          else
            view.data(x.data);
          
        });

        c.render();

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      },
      getC: function(){
        return c;
      }

    };
  }
});
