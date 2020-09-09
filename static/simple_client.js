function renderPage() {
    document.getElementById('query_form').addEventListener('submit', handleSubmit);
    document.getElementById('result').style.display = 'none';
}

function handleSubmit(event) {
    event.preventDefault();

    let text = document.getElementById('query');
    fetchFromServer({query: text.value},
                    query_result => displayQueryResult(query_result));
}

function displayQueryResult(query_result) {
    document.getElementById('result').style.display = 'block';
    document.getElementById('result:query').innerHTML = '<code>' + sanitizeText(query_result.query) + '</code>';
    document.getElementById('result:success').innerHTML = '<i>' + query_result.success.toString() + '</i>';
    document.getElementById('result:query_after_call').innerHTML = '<code>' + sanitizeText(query_result.query_after_call) + '</code>';
    document.getElementById('result:error').innerHTML = '<i><code>' + sanitizeText(query_result.error) + '</code></i>';
    document.getElementById('result:printed_output').innerHTML = '<b><font face="Inconsolata">' + sanitizeText(query_result.printed_output) + '</></b>';
    document.getElementById('result:vars').innerHTML = '&nbsp';
    if (query_result.success === true) {
        let table = document.createElement('table');
        table.style.border = '1px solid black';
        table.style['border-spacing'] = '0px';
        for (const one_var of query_result.vars) {
            var row = table.insertRow();
            row.vAlign = 'top';
            var td1 = row.insertCell();
            td1.style.border = '1px solid black';
            td1.style.padding = '5px';
            td1.innerHTML = '<b><code>' + sanitizeText(one_var['var']) + '</code></b>';
            var td2 = row.insertCell();
            td2.style.border = '1px solid black';
            td2.style.padding = '5px';
            td2.innerHTML = '<code>' + sanitizeText(one_var.value) + '</code>';
        }
        let result_vars_elem = document.getElementById('result:vars');
        while (result_vars_elem.firstChild) {
            result_vars_elem.firstChild.remove();
        }
        result_vars_elem.appendChild(table);
        document.getElementById('result:query_after_call').innertHTML = '<code>' + sanitizeText(query_result.query_after_call) + '</code>';
    } else if (query_result.success === false) {
        // do nothing
    } else if (query_result.success === 'error') {
        // do nothing
    } else {
        alert('Impossible code from server: ' + JSON.stringify(query_result));
    }
}

// Send a request to the server and schedule a callback.
function fetchFromServer(request, callback) {
    // callback should take a single arg, the response from the server,
    fetch('/json',
          {method: 'POST',
           headers: {'Content-Type': 'application/json'},
           body: JSON.stringify(request),
           mode: 'cors',                  // Don't need?
           cache: 'no-cache',             // Don't need?
           credentials: 'same-origin',    // Don't need?
           redirect: 'follow',            // Don't need?
           referrerPolicy: 'no-referrer', // Don't need?
          })
        .then(response => response.json())
        .then(callback)
        // TODO: the following doesn't capture enough information;
        //       there is interesting information in the console log
        //       such as error code 500 or ERR_CONNECTION_REFUSED
        .catch(err => {
            alert('***fetch ' + JSON.stringify(request) + ': ' + err) });
}

// Sanitize a string, allowing tags to not cause problems
function sanitizeText(raw_str) {
    // There shouldn't be a need for .replace(/ /g, '&nbsp;') if CSS
    // has white-space:pre ... but by experiment, it's needed.
    // TODO: remove the '<br/>' insertion and put it into extract_color.pl.
    return (raw_str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;')
        .replace(/\n/g, '<br/>')  // TODO: remove - not needed?
        .replace(/\s/g, '&nbsp;');  // TODO: add test for tabs in source
}
