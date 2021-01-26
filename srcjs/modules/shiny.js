import "widgets";

const getChart = (id) => {
  let htmlWidgetsObj = HTMLWidgets.find("#" + id);

  let chart;

  if (htmlWidgetsObj === undefined) return chart;

  if (htmlWidgetsObj === null) return chart;

  chart = htmlWidgetsObj.getChart();

  return chart;
};

const getView = (id, index) => {
  let htmlWidgetsObj = HTMLWidgets.find("#" + id);

  let view;

  if (htmlWidgetsObj === undefined) return view;

  if (htmlWidgetsObj === null) return view;

  if (typeof index == "string") {
    let views = htmlWidgetsObj.getViews();
    index = views.findIndex((data) => data.id == index);

    // cannot find view with index name
    if (index < 0) return view;
  }

  view = htmlWidgetsObj.getView(index);

  return view;
};

export { getChart, getView };
