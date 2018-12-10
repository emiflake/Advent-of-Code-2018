const ids = require("fs").readFileSync("ids.txt").toString().split("\n");

const diff = (a, b) => {
    let count = 0;
    for (let i = 0; i < Math.max(a.length, b.length); i++) {
        count += a[i] != b[i];
    }
    return count;
}

const noDiff = (a, b) => {
    let res = "";
    for (let i = 0; i < Math.max(a.length, b.length); i++) {
        if (a[i] === b[i]) { res += a[i]; }
    }
    return res;
}

let results = [];

for ( const idA of ids ) {
    for (const idB of ids) {
        if (idA == idB) continue;
        results.push({
            diff: diff(idA, idB), 
            comparison: {
                a: idA, 
                b: idB
            }
        });
    }
}

const [best] = results.sort((a, b) => a.diff - b.diff);

console.log(noDiff(best.comparison.a, best.comparison.b))