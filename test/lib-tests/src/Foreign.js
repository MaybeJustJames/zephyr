function add_foreign(x, y) {
  return x + y;
}

function mult_foreign(x, y) {
  return x * y;
}

export function add(x) {
  return function (y) {
    return add_foreign(x, y);
  };
}

export function mult(x) {
  return function (y) {
    return mult_foreign(x, y);
  };
}
