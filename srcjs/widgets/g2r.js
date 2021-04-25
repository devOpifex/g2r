import "widgets";
import { Chart, registerTheme } from "@antv/g2";
import { registerInteractions } from "../modules/interactions.js";
import { crosstalkFilter, crosstalkSelect } from "../modules/crosstalk.js";
import { observeActions } from "../modules/action.js";
import { plot } from "../modules/plot.js";
import { getChart, getView } from "../modules/shiny.js";
import { actions } from "../modules/actions";
import { registerShapes } from "../modules/shapes.js";

registerShapes();

HTMLWidgets.widget({
  name: "g2r",

  type: "output",

  factory: function (el, width, height) {
    let c;
    // Crosstalk init
    let ctFilter = new crosstalk.FilterHandle();
    let ctSelection = new crosstalk.SelectionHandle();

    return {
      renderValue: function (x) {
        registerInteractions(x.registerInteractions);

        // keep autofit for responsiveness
        x.chartOpts.container = el.id;

        if (c) {
          c.destroy();
        }

        c = new Chart(x.chartOpts);

        // crosstalk
        crosstalkFilter(c, ctFilter);
        crosstalkSelect(c, ctSelection, x.crosstalk_select);

        // main plot function
        plot(c, x, el);

        if (x.motif) c.theme(x.motif);

        if (x.crosstalk_group) {
          // set group
          ctFilter.setGroup(x.crosstalk_group);
          ctSelection.setGroup(x.crosstalk_group);

          c.on("element:click", function (ev) {
            ctSelection.set([ev.data.data["CROSSTALK_KEYS"]]);
          });

          c.views.forEach((view) => {
            view.on("afterpaint", function () {
              let data = view.filteredData;
              let indices = [];
              data.map((row) => indices.push(row["CROSSTALK_KEYS"]));

              ctFilter.set(indices);
            });
          });

          c.on("reset-button:click", () => {
            ctFilter.clear();
          });
        }
        actions(c, x.actions);
        c.render();

        // Sometimes the viz is badly drawn until resize
        // this forces resize by 1px on the height
        c.changeSize(width, height - 1);
      },

      resize: function (width, height) {
        // no need in g2.js
        c.changeSize(width, height);
      },

      getChart: function () {
        return c;
      },

      getView: function (index) {
        return c.views[index];
      },

      getViews: function (index) {
        return c.views;
      },
    };
  },
});

if (HTMLWidgets.shinyMode) {
  // Figure
  Shiny.addCustomMessageHandler("render", (x) => {
    let c = getChart(x.id);

    plot(c, x);

    c.render(x.update);
  });

  // remove figure
  Shiny.addCustomMessageHandler("remove-figure", (msg) => {
    let c = getChart(msg.id);
    let v = getView(msg.id, msg.index);

    c.removeView(v);
  });

  // Change Data
  Shiny.addCustomMessageHandler("change-data", (msg) => {
    let c = getChart(msg.id);
    c.changeData(msg.data);
  });
}

// observe actions
observeActions();
