exports.unsafeTrace = function(x) {
  return function(y) {
    console.log(x);
    return y;
  };
};
