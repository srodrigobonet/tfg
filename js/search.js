// Mostrar la rueda de carga---------------------------------------------------------------------
function showLoadingSpinner() {
    const loadingSpinner = document.getElementById('loading-spinner');
    loadingSpinner.classList.remove('hidden');
}

// Ocultar la rueda de carga
function hideLoadingSpinner() {
    const loadingSpinner = document.getElementById('loading-spinner');
    loadingSpinner.classList.add('hidden');
} //---------------------------------------------------------------------------------------------

// Función para actualizar la leyenda------------------------------------------------------------
function updateLegend(filter) {
    const legend = document.getElementById('legend');
    const dLegendItems = document.querySelectorAll('.d-legend');
    const fLegendItems = document.querySelectorAll('.f-legend');

    // Ocultar todas las líneas inicialmente
    dLegendItems.forEach(item => item.classList.add('hidden'));
    fLegendItems.forEach(item => item.classList.add('hidden'));

    if (['diagnosticos', 'ChapterCIAP1', 'CIAP1', 'ChapterCIE10', 'CIE10', 'SNOMED'].includes(filter)) {
        dLegendItems.forEach(item => item.classList.remove('hidden'));
    } else if (['farmacos', 'LevelATC', 'ATC', 'Aemps', 'RxNorm'].includes(filter)) {
        fLegendItems.forEach(item => item.classList.remove('hidden'));
    } else if (filter === '') {
        // Si no hay filtro, mostrar todo
        dLegendItems.forEach(item => item.classList.remove('hidden'));
        fLegendItems.forEach(item => item.classList.remove('hidden'));
    }

    const anyVisible = Array.from(legend.querySelectorAll('.legend-item')).some(item => !item.classList.contains('hidden'));
    if (anyVisible) {
        legend.classList.remove('hidden');
    } else {
        legend.classList.add('hidden');
    }
} //---------------------------------------------------------------------------------------------

// Búsqueda de nodos y aristas por coincidencia parcial de label, code, y vocab------------------
function performSearch() {
    const labelValue = document.getElementById('search-label').value.trim().toLowerCase();
    const codeValue = document.getElementById('search-code').value.trim().toLowerCase();
    const classValue = document.getElementById('class-filter').value;

    let vocab = '';
    if (classValue === 'diagnosticos') {
        vocab = 'ChapterCIAP1,CIAP1,ChapterCIE10,CIE10,SNOMED';
    } else if (classValue === 'farmacos') {
        vocab = 'LevelATC,ATC,Aemps,RxNorm'; 
    } else {
        vocab = classValue; 
    }
    updateLegend(classValue);
    if (labelValue || codeValue || vocab) {
        showLoadingSpinner();
        const searchParams = new URLSearchParams({
            term: labelValue,
            code: codeValue,
            vocab: vocab
        });

        fetch(`/api/search?${searchParams.toString()}`)
            .then(response => response.json())
            .then(data => {
                hideLoadingSpinner();
                if (data.nodes.length > 0) {
                    updateGraph(data);
                } else {
                    Swal.fire({
                        title: '!',
                        text: 'No se ha encontrado ninguna coincidencia.',
                        confirmButtonText: 'Aceptar',
                        confirmButtonColor: '#6d6d6d',
                    });
                }
            })
            .catch(error => {
                hideLoadingSpinner();
                console.error('Error en la búsqueda:', error);
            });
    } else {
        Swal.fire({
            title: 'Campos vacíos',
            text: 'Por favor, escriba en al menos uno de los campos de búsqueda.',
            icon: 'warning',
            confirmButtonText: 'Aceptar',
            confirmButtonColor: '#6d6d6d',
        });
    }
}

// Búsqueda al hacer clic en el botón
document.getElementById('search-button').addEventListener('click', performSearch);

// Búsqueda al presionar Enter en los campos de búsqueda
document.addEventListener('keydown', function(event) {
    if (event.key === 'Enter') {
        event.preventDefault();
        performSearch(); 
    }
});

// Limpiar la búsqueda y vaciar el grafo
document.getElementById('clear-button').addEventListener('click', function() {
    document.getElementById('search-label').value = '';
    document.getElementById('search-code').value = '';
    document.getElementById('class-filter').value = '';
    document.getElementById('legend').classList.add('hidden');
    eraseMode = false;
    eraseButton.style.backgroundColor = '#0F5C99';
    cy.elements().remove();
}); //-------------------------------------------------------------------------------------------
