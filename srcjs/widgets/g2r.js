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

        // keep autofit for responsiveness
        c = new Chart({
          container: el.id,
          autoFit: true,
        });

        if(x.data)
          c.data(x.data);

        if(x.scale)
          c.scale(x.scale);

        // loop over figures
        x.views.forEach(function(v){
          let view = c.createView();
          let e;

          // view type
          if(v.type == "point")
            e = view.point();

          if(v.type == "interval")
            e = view.interval();

          if(v.type == "line")
            e = view.line();

          if(v.type == "area")
            e = view.area();

          if(v.type == "polygon")
            e = view.polygon();

          if(v.type == "schema")
            e = view.schema();

          if(v.type == "edge")
            e = view.edge();

          if(v.type == "path")
            e = view.path();
          
          // position
          e.position(v.position);

          // color
          if(v.color)
            e.color(v.color);

          // shape
          if(v.shape)
            e.shape(v.shape);

          // size
          if(v.size)
            e.size(v.size);
          
          // data
          if(v.data)
            view.data(v.data);
          else
            view.data(x.data);
          
          // add view to array for proxy
          views.push(view); 
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
