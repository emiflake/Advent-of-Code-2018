const fs = require("fs");

const getCarts = input => input.split("\n").map((line, y) => 
    line.split("").map((c, x) => ({x, y, c})).filter(({c}) => c.match(/[\<\>\^v]/))).reduce((a, b) => a.concat(b));

const cartToTrack = ({yvel, xvel}) => {
    if (xvel == 0) return '|';
    else if (yvel == 0) return '-';
};

const parseCart = ({c, x, y}) => ({
    '>': { xvel: 1, yvel: 0, x, y, turnNext: turnNext(0)},
    '<': { xvel: -1, yvel: 0, x, y, turnNext: turnNext(0)},
    'v': { xvel: 0, yvel: 1, x, y, turnNext: turnNext(0)},
    '^': { xvel: 0, yvel: -1, x, y, turnNext: turnNext(0)}
})[c];

const turnNext = n => ({value: n % 3, next: () => turnNext(n + 1)});

const parse = str => str.split("\n").map((line, y) => line.split("").map((c, x) => {
    if (c.match(/[\<\>\^v]/)) {
        return cartToTrack(parseCart({c, x, y}));
    } else {
        return c;
    }
}));

const trackAt = track => (x, y) => { 
    if (y >= track.length || y < 0) return ' ';
    const l = track[y];

    if (x >= l.length || x < 0) return ' ';
    return l[x];
}

const sortCarts = carts =>
    carts.sort(({x: ax, y: ay}, {x: bx, y: by}) => 
        ax == bx ? ay - by : ax - bx);

const representCart = ({xvel, yvel}) => ({
    "1, 0": '>',
    "-1, 0": '<',
    "0, 1": 'v',
    "0, -1": '^'
})[xvel+", "+yvel];


const printState = (accessor, carts) => {

    const dim = {x: 300, y: 300};

    let str = []
    for (let y = 0; y < dim.y; y++) {
        let l = "";
        for (let x = 0; x < dim.x; x++) {
            l += accessor(x, y);
        }
        str.push(l);
    }

    for (const {xvel, yvel, x, y} of carts.filter(cart => !cart.removed)) {
        str[y] = str[y].split("").map((c, i) => i==x ? representCart({xvel, yvel}) : c).join("")
    }

    return str.join("\n");
};

const main = () => {
    const input = (fs.readFileSync("./input.txt")).toString();


    const track = parse(input).map(x => x.filter(z => z!='\r'));
    const accessor = trackAt(track);

    let carts = getCarts(input).map(parseCart).map((cart, id) => ({...cart, id}));

    let iteration = 0;
    let collcount = 0;

    while (iteration++ < 20000) {
        carts = sortCarts(carts);

        for ( let cart of carts ) {
            if (cart.removed) continue;
            let cell = accessor(cart.x, cart.y);

            if (cell == '/') {
                [cart.xvel, cart.yvel] = [-cart.yvel, -cart.xvel];
            }

            if (cell == '\\') {
                [cart.xvel, cart.yvel] = [cart.yvel, cart.xvel];
            }
            
            if (cell == '+') {
                const turn = cart.turnNext.value;


                const turnRight = (cart) => {
                    const {xvel, yvel} = cart;
                    return Object.assign({}, cart, {xvel: yvel, yvel: -xvel});
                };
                
                const turnLeft = (cart) => {
                    const {xvel, yvel} = cart;
                    return Object.assign({}, cart, {xvel: -yvel, yvel: xvel});
                };

                if (turn == 0) {
                    [cart.xvel, cart.yvel] = [cart.yvel, -cart.xvel];
                } else if (turn == 1) {
                    // do nothing
                } else if (turn == 2) {
                    [cart.xvel, cart.yvel] = [-cart.yvel, cart.xvel];
                }

                cart.turnNext = cart.turnNext.next();
            }

            cart.x += cart.xvel;
            cart.y += cart.yvel;

            for (let other of carts) {
                if (other.x == cart.x && other.y == cart.y && other.id != cart.id && !other.removed) {
                    const survivors = carts.filter(cart => !cart.removed).length;
                    console.log("Collision at", iteration, "pos:",cart.x, cart.y)

                    other.removed = true;
                    cart.removed = true;
                    
                    if (survivors == 1) {
                        const {x, y} = carts.find(cart => !cart.removed);
                        console.log("Winner: ", x, y);
                    }
                }
            }
        }
    }
};

main();