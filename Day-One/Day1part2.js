const freqs = require("fs").readFileSync("freq.txt").toString().split("\n").map(eval);

let past = [];
let current = 0;
let i = 0;
let found = false;

while (true) {
    past.push(current);
    const freq = freqs[i % freqs.length];
    
    current = current + freq;
    
    if (past.indexOf(current) > 0) {
        console.log(current);
        break;
    }
    i++;
}




