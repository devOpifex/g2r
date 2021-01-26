const check = (options, name) => {
  return typeof options[name] == "object" && options[name].length == 2;
};

const checkType = (options) => {
  return typeof options == "object" && options.length == 2;
};

export { check, checkType };
