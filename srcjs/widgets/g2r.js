import 'widgets';
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
import { crosstalkFilter, crosstalkSelect } from '../modules/crosstalk.js'

HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    let c;
    // Crosstalk init
    let ctFilter = new crosstalk.FilterHandle();
    let ctSelection = new crosstalk.SelectionHandle();

    return {

      renderValue: function(x) {

        if(x.theme)
          registerTheme(x.chartOpts.theme, x.theme);

        registerInteractions(x);

        // keep autofit for responsiveness
        x.chartOpts.container = el.id;
        c = new Chart(x.chartOpts);

        // crosstalk
        crosstalkFilter(c, ctFilter);
        crosstalkSelect(c, ctSelection, x.crosstalk_select.stroke, x.crosstalk_select.fill);

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

        if(x.crosstalk_group){

          // set group
          ctFilter.setGroup(x.crosstalk_group);
          ctSelection.setGroup(x.crosstalk_group);

          c.on("element:click", function(ev){
            ctSelection.set([ev.data.data['CROSSTALK_KEYS']]);
          });

          c.views[0].on("afterpaint", function(){
            let data = c.views[0].filteredData;
            let indices = [];
            data.map((row) => indices.push(row['CROSSTALK_KEYS']));

            if(indices.length == data.length)
              return ;

            ctFilter.set(indices);
          })

        }

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


