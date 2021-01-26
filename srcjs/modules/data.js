const getData = (globalData, layerData) => {
  if (layerData) return layerData;

  return globalData;
};

export { getData };
