const annotate = (view, options) => {
  if (options === undefined) return;

  options.forEach(function (info) {
    if (info.type == "text") view.annotation().text(info.opts);

    if (info.type == "image") view.annotation().image(info.opts);

    if (info.type == "arc") view.annotation().arc(info.opts);

    if (info.type == "line") view.annotation().line(info.opts);

    if (info.type == "region") view.annotation().region(info.opts);

    if (info.type == "regionFilter") view.annotation().regionFilter(info.opts);

    if (info.type == "dataMarker") view.annotation().dataMarker(info.opts);

    if (info.type == "dataRegion") view.annotation().dataRegion(info.opts);

    if (info.type == "shape") view.annotation().shape(info.opts);

    if (info.type == "html") view.annotation().html(info.opts);
  });
};

export { annotate };
