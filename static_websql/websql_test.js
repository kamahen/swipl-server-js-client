'use strict';

// Simple client for testing WebSQL

var db = undefined

// Called by <body onload="renderPage();">
async function renderPage() {
    document.getElementById('buy_lot').addEventListener('submit', handleBuySubmit);
    if (window.openDatabase) {
        db = openDatabase("mykabu_db",
                          "0.1", // version
                          "A database of stock transactions",
                          10 * 1024 * 1024 // size: 10MB
                         );
        db.transaction(
            (t) => t.executeSql(
                'CREATE TABLE IF NOT EXISTS buy_lots (' +
                    'id INTEGER PRIMARY KEY ASC AUTOINCREMENT, ' +
                    'ticker VARYING CHARCTER(40), ' + // TODO: TEXT ?
                    'timestamp DATETIME, ' +
                    'shares INTEGER, ' +
                    'price DECIMAL(15,5), ' +
                    'notes TEXT, ' +
                    'broker VARYING CHARCTER(40)' +
                    ')'));
        // db.transaction(
        //     (t) => t.executeSql(
        //         'INSERT INTO buy_lots(ticker,timestamp,shares,price,notes,broker) ' +
        //             'VALUES(?,?,?,?,?,?)',
        //         ['GOOG','2021-01-02 10:11:12.345',10,1050.23,'first item','Fidelity']));
        show_buy_lots();
    } else {
        alert('WebSQL is not supported by your browser!');
    }
}

// Handler for buy form's "submit" button
async function handleBuySubmit(event) {
    event.preventDefault();
    db.transaction(
        (t) => t.executeSql(
            'INSERT INTO buy_lots(ticker,timestamp,shares,price,notes,broker) ' +
                'VALUES(?,?,?,?,?,?)',
            [document.getElementById('buy.ticker').value,
             document.getElementById('buy.date').value,
             document.getElementById('buy.shares').value,
             document.getElementById('buy.price').value,
             document.getElementById('buy.notes').value,
             document.getElementById('buy.broker').value]));
    show_buy_lots();
}

// Display the buy_lots contents
function show_buy_lots() {
    db.transaction(
        (t) => t.executeSql(
            'SELECT id,ticker,timestamp,shares,price,notes,broker ' +
                'from buy_lots ORDER BY id',
            [], // substitute variables for '?'
            (t, data) => show_buy_lots_handler(data)));
}

// Callback from SELECT ... from buy_lots that displays the results
function show_buy_lots_handler(data) {
    var table = document.createElement('table');
    if (data) {
        var hdr = table.createTHead().insertRow();
        for (const col of Object.keys(data.rows[0])) {
            var c = hdr.insertCell();
            c.innerHTML = '<B>' + sanitizeText(col) + '</B>';
        }
        for (const data_row of data.rows) {
            var row = table.insertRow();
            for (const col of Object.values(data_row)) {
                var c = row.insertCell();
                c.innerHTML = sanitizeText('' + col);
            }
        }
    }
    replaceChildWith('buy_lots', table);
}

// Clear an element
function deleteAllChildren(elem) {
    while (elem.firstChild) {
        elem.firstChild.remove();
    }
}

// Clear an element and replace with a single child
function replaceChildWith(id, new_child) {
    var elem = document.getElementById(id);
    // elem.replaceChild(new_child, elem.firstChild);
    deleteAllChildren(elem);
    elem.appendChild(new_child);
}

// Sanitize a string, allowing tags to not cause problems
function sanitizeText(raw_str) {
    // There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
    // has white-space:pre ... but by experiment, it's needed.
    // TODO: remove the '<br/>' insertion and put it into extract_color.pl.
    return raw_str ? (raw_str
                      .replace(/&/g, '&amp;')
                      .replace(/</g, '&lt;')
                      .replace(/>/g, '&gt;')
                      .replace(/"/g, '&quot;')
                      .replace(/'/g, '&apos;')
                      .replace(/\n/g, '<br/>')  // TODO: remove - not needed?
                      .replace(/\s/g, '&nbsp;'))  // TODO: add test for tabs in source
        : raw_str;
}

