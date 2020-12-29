import 'widgets';

const getProxy = (id) => {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var chart;

  if (typeof htmlWidgetsObj != 'undefined') {
    chart = htmlWidgetsObj.getChart();
  }

  return(chart);
}

const getView = (id, index) => {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var chart;

  if (typeof htmlWidgetsObj != 'undefined') {
    chart = htmlWidgetsObj.getView(index);
  }

  return(chart);
}

export { getProxy, getView };
