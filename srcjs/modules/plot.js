import { addFigure } from "./figures.js";
import { makeCoords } from "./coords.js";
import { gaugeFigure } from "./gauges.js";
import { getData } from "./data.js";
import { annotate } from "./annotate.js";
import { captureEvents } from "./events.js";
import { interactions } from "./interactions.js";
import { facetFactory } from "./facet.js";
import { getView } from "./shiny.js";

const plot = (c, x, el) => {
  // data
  if (x.crosstalk_group) c.data(x.data);

  // scale
  if (x.scale) c.scale(x.scale);

  // Coordinates
  makeCoords(
    c,
    x.coord,
    x.coordRotate,
    x.coordScale,
    x.coordReflect,
    x.coordTranspose
  );

  // remove interactions
  if (x.rmInteractions)
    x.rmInteractions.forEach((inter) => {
      c.removeInteraction(inter);
    });

  // add interactions
  if (x.interactions)
    x.interactions.forEach((inter) => {
      c.interaction(inter);
    });

  // events
  captureEvents(c, el, x.events);

  if (x.axis) {
    x.axis.forEach(function (ax) {
      c.axis(ax.column, ax.opts);
    });
  }

  if (x.legend)
    x.legend.forEach(function (l) {
      c.legend(l.column, l.opts);
    });

  if (x.tooltip) c.tooltip(x.tooltip);

  if (x.style) c.style(x.style);

  // loop over figures
  if (x.facet) {
    // data
    c.data(x.data);

    // views
    x.facet.opts.eachView = facetFactory(x.views);

    c.facet(x.facet.type, x.facet.opts);
  } else {
    x.views.forEach(function (layer, index) {
      let view;

      // retrieve view instead of create if id is passed
      if (layer.conf && layer.conf.id) view = getView(x.id, layer.conf.id);

      // if not found create one
      if (view === undefined || view === null) view = c.createView(layer.conf);

      // otherwise the label is written on top of the previous
      if (index != 0 && !x.coord) {
        let positions = layer.position.split("*");
        positions.forEach(function (pos) {
          let opts = {
            label: {
              style: {
                opacity: 0,
              },
            },
          };

          // skip this if the axis is hidden
          if (x.axis) {
            for (let i = 0; i < x.axis.length; i++) {
              if (x.axis[i].column == pos && x.axis[i].opts === false) {
                return;
              }
            }
          } else {
            // draw labels with 0 opacity
            // this avoids smudge labels
            view.axis(pos, opts);
          }
        });
      }

      annotate(view, x.annotations);

      interactions(view, layer.interaction);

      let figure = addFigure(view, layer.type);
      gaugeFigure(figure, layer);

      if (x.slider) view.option("slider", x.slider);

      if (x.scrollbar) view.option("scrollbar", x.scrollbar);

      if (layer.conf && layer.conf.id)
        view.changeData(getData(x.data, layer.data));
      else view.data(getData(x.data, layer.data));

      if (x.motif) view.theme(x.motif);
    });
  }
};

export { plot };
