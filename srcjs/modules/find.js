import 'widgets';

const getProxy = (id) => {
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var chart;

  if (typeof htmlWidgetsObj != 'undefined') {
    chart = htmlWidgetsObj.getC();
  }

  return(chart);
}

export { getProxy };
