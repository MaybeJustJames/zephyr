function add (x, y) {
    return x + y;
}

function mult (x, y) {
    return x * y;
}

exports.add = function(x) {
    return function (y) {
        return add (x, y);
    }
}

exports.mult = function(x) {
    return function (y) {
        return mult(x, y);
    }
}
