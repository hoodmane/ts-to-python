export function groupBy<T, K extends keyof any>(
  list: Iterable<T>,
  getKey: (item: T) => K,
): Record<K, T[]> {
  const gen = groupByGen(getKey);
  for (const x of list) {
    gen.next(x);
  }
  return gen.done();
}

export type WrappedGen<T, R> = { next: (T) => void; done: () => R };

const doneToken = {};

export function groupByGen<T, K extends keyof any>(
  getKey: (item: T) => K,
): WrappedGen<T, Record<K, T[]>> {
  const g = groupByGenHelper(getKey);
  g.next();
  return {
    next: (x) => {
      g.next(x);
    },
    done: () => g.throw(doneToken).value,
  };
}

function* groupByGenHelper<T, K extends keyof any>(getKey: (item: T) => K) {
  const result = Object.create(null) as Record<K, T[]>;
  try {
    while (true) {
      const currentItem: T = yield result;
      const group = getKey(currentItem);
      if (!result[group]) {
        result[group] = [];
      }
      result[group].push(currentItem);
    }
  } catch (e) {
    if (e === doneToken) {
      return result;
    }
    throw e;
  }
}

export function split<T, S extends T>(
  list: Iterable<T>,
  cond: (t: T) => t is S,
): [S[], T[]] {
  const ss: S[] = [];
  const ts: T[] = [];
  for (let x of list) {
    if (cond(x)) {
      ss.push(x);
    } else {
      ts.push(x);
    }
  }
  return [ss, ts];
}

export function popElt<T>(s: Set<T>): T | undefined {
  const res = s[Symbol.iterator]().next();
  if (res.done) {
    return undefined;
  }
  s.delete(res.value);
  return res.value;
}
