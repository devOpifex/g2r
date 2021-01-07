import 'widgets';

const getChart = (id) => {
  let htmlWidgetsObj = HTMLWidgets.find("#" + id);

  let chart;

  if (typeof htmlWidgetsObj != 'undefined') {
    chart = htmlWidgetsObj.getChart();
  }

  return(chart);
}

const getView = (id, index) => {
  let htmlWidgetsObj = HTMLWidgets.find("#" + id);

  let view;

  if (typeof htmlWidgetsObj != 'undefined') {
    
    if(typeof index == "string"){
      let views = htmlWidgetsObj.getViews();
      index = views.findIndex((data) => data.id == index);

      // cannot find view with index name
      if(index < 0)
        return view;
    }

    view = htmlWidgetsObj.getView(index);

  }

  return(view);
}

export { getChart, getView };
