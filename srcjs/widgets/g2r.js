import 'widgets';
import { Chart } from '@antv/g2';

HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    let c, 
        views = [];

    return {

      renderValue: function(x) {

        c = new Chart({
          container: el.id,
          autoFit: true,
        });

        if(x.data)
          c.data(x.data);

        x.views.forEach(function(v){
          console.log(v);
          let view = c.createView();

          view.interval().position(v.position);
          
          if(v.hasOwnProperty('data'))
            view.data(v.data);
          else
            view.data(x.data);
          
          views.push(view); // add view to array for proxy
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
