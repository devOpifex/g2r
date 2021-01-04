import 'widgets';
import 'crosstalk';
import { Chart, registerTheme } from '@antv/g2';
import { makeFigure } from '../modules/makeFigure.js'; 
import { makeCoords } from '../modules/makeCoords.js'; 
import { tuneFigure } from '../modules/tuneFigure.js';
import { assignData } from '../modules/assignData.js';
import { annotate } from '../modules/annotate.js';
import { captureEvents } from '../modules/events.js';
import { 
  globalInteractions, 
  interactions, 
  rmInteractions,
  registerInteractions 
} from '../modules/interactions.js';
import { getProxy, getView } from '../modules/shiny.js';
import { facetFactory } from '../modules/facet.js';

HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    let c, sel_handle;

    sel_handle = new crosstalk.SelectionHandle();

    sel_handle.on("change", function(e) {
      let selected = e.value;
      c.views.forEach((v) => {
        v.filter('CROSSTALK_KEYS', (key) => selected.includes(key))
      })
      c.render();
    });

    return {

      renderValue: function(x) {

        if(x.crosstalk_group)
          sel_handle.setGroup(x.crosstalk_group);

        if(x.theme)
          registerTheme(x.chartOpts.theme, x.theme);

        registerInteractions(x);

        // keep autofit for responsiveness
        x.chartOpts.container = el.id;
        c = new Chart(x.chartOpts);

        if(x.data)
          c.data(x.data);

        // scale
        if(x.scale)
          c.scale(x.scale);

        // Coordinates
        makeCoords(c, x);

        // interactions
        rmInteractions(c, x);
        globalInteractions(c, x);

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
          x.facet.opts.eachView = facetFactory(x);

          c.facet(x.facet.type, x.facet.opts);

        } else {
          x.views.forEach(function(v){
            let view = c.createView();

            annotate(view, x);

            // interactions
            interactions(view, v);
  
            let figure = makeFigure(view, v.type);
            tuneFigure(figure, v);
  
            // data
            assignData(view, v, x)
            
          });
        }

        if(x.slider)
          c.option('slider', x.slider);

        if(x.scrollbar)
          c.option('scrollbar', x.scrollbar);

        c.render();

      },

      resize: function(width, height) {
        // no need in g2.js
        c.changeSize(width, height);
      },

      getChart: function(){
        return c;
      },

      getView: function(index){
        return c.views[index];
      }

    };
  }
});


