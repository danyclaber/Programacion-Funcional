// Construye la representación del polinomio en orden ascendente: coef x^0 + coef x^1 + ...
function construirPolinomio(coefs) {
    return coefs
        .map((c, i) => {
            if (c === 0) return '';
            const coef = (c === 1 && i !== 0) ? '' : (c === -1 && i !== 0) ? '-' : c;
            if (i === 0) return `${coef}`;
            if (i === 1) return `${coef}x`;
            return `${coef}x^${i}`;
        })
        .filter(Boolean)
        .join(' + ')
        .replace(/\+ -/g, '- ') || '0';
}

// Actualiza la representación al escribir en los inputs
function actualizarRepresentacion(inputId, repId) {
    const input = document.getElementById(inputId).value;
    const coefs = input.split(',').map(n => parseInt(n.trim()) || 0);
    document.getElementById(repId).textContent = construirPolinomio(coefs);
}

// Listeners para actualizar representaciones en tiempo real
document.getElementById('p1').addEventListener('input', () => {
    actualizarRepresentacion('p1', 'rep1');
});

document.getElementById('p2').addEventListener('input', () => {
    actualizarRepresentacion('p2', 'rep2');
});

function operar(accion) {
    const p1 = document.getElementById('p1').value.trim();
    const p2 = document.getElementById('p2').value.trim();

    // Validaciones
    if (accion === 'derivar1' && !p1) {
        alert("Por favor ingresa el polinomio 1 para derivar");
        return;
    }
    if (accion === 'derivar2' && !p2) {
        alert("Por favor ingresa el polinomio 2 para derivar");
        return;
    }
    if ((accion === 'sumar' || accion === 'multiplicar') && (!p1 || !p2)) {
        alert("Por favor ingresa ambos polinomios");
        return;
    }

    const params = new URLSearchParams();

    // Para derivar solo uno, enviamos solo ese y 'accion' = 'derivar'
    if (accion === 'derivar1') {
        params.append('p1', p1);
        params.append('accion', 'derivar');
        params.append('polinomio', 'p1'); // para backend saber cuál derivar
    } else if (accion === 'derivar2') {
        params.append('p2', p2);
        params.append('accion', 'derivar');
        params.append('polinomio', 'p2');
    } else {
        // Para sumar y multiplicar se envían ambos y la acción
        if (p1) params.append('p1', p1);
        if (p2) params.append('p2', p2);
        params.append('accion', accion);
    }

    fetch('http://localhost:8000/api', {
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        body: params.toString()
    })
        .then(response => response.json())
        .then(data => {
            if (data.resultado) {
                console.log(data.resultado);
                if (Array.isArray(data.resultado)) {
                    document.getElementById('resultado').textContent = construirPolinomio(data.resultado);
                } else {
                    document.getElementById('resultado').textContent = data.resultado;
                }
            } else {
                document.getElementById('resultado').textContent = "Error inesperado del servidor";
            }
        })
        .catch(err => {
            document.getElementById('resultado').textContent = "Error en la conexión al servidor";
            console.error(err);
        });
}

// Función para limpiar un input específico
function limpiarInput(id) {
    document.getElementById(id).value = '';
    if (id === 'p1') document.getElementById('rep1').textContent = '';
    if (id === 'p2') document.getElementById('rep2').textContent = '';
    document.getElementById('resultado').textContent = '';
}

// Función para limpiar ambos inputs y resultados
function limpiarTodo() {
    limpiarInput('p1');
    limpiarInput('p2');
    document.getElementById('resultado').textContent = '';
}

