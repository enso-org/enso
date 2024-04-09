function Natural() {
    this.x = 2;
};
Natural.prototype.next = function() {
    return this.x++;
};

function Filter(number) {
    this.number = number;
    this.next = null;
    this.last = this;
}
Filter.prototype.acceptAndAdd = function(n) {
  var filter = this;
  var sqrt = Math.sqrt(n);
  for (;;) {
      if (n % filter.number === 0) {
          return false;
      }
      if (filter.number > sqrt) {
          break;
      }
      filter = filter.next;
  }
  var newFilter = new Filter(n);
  this.last.next = newFilter;
  this.last = newFilter;
  return true;
};

function Primes(natural) {
    this.natural = natural;
    this.filter = null;
}
Primes.prototype.next = function() {
    for (;;) {
        var n = this.natural.next();
        if (this.filter === null) {
            this.filter = new Filter(n);
            return n;
        }
        if (this.filter.acceptAndAdd(n)) {
            return n;
        }
    }
};

function globalPrimes(upto) {
    var primes = new Primes(new Natural());
    for (var cnt = 1;; cnt++) {
        res = primes.next();
        if (cnt >= upto) {
            return res;
        }
    }
}
