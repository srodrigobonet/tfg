const relationshipMapping = {
    "Isa": "tipo",
    "hasATC": "ATC",
    "hasActiveSub": "sustancia activa",
    "hasCIAP1": "CIAP-1",
    "hasFindingSite": "localización anatómica",
    "hasICD10": "CIE-10",
    "hasRxNorm": "RxNorm",
    "hasSNOMED": "SNOMED",
    "include": "incluye",
    "laterality": "lateralidad",
    "synonym": "sinónimo"
};

// Mapear relaciones según el nivel del nodo
function mapRelation(type, direction, level) {
    if (level === 1 && direction === 'out' && type === 'incluye') return 'capítulo';
    if (level === 2 && direction === 'out' && type === 'incluye') return 'sección';
    if (level === 2 && direction === 'in' && type === 'incluye') return 'terminología';
    if (level === 3 && direction === 'out' && type === 'incluye') return 'más específico';
    if (level === 3 && direction === 'in' && type === 'incluye') return 'capítulo';
    if (level === 4 && direction === 'in' && type === 'incluye') return 'sección';
    if (level === 4 && direction === 'in' && type === 'tipo') return 'más específico';
    if (level >= 5 && direction === 'in' && type === 'tipo') return 'más específico';
    if (level >= 5 && direction === 'out' && type === 'tipo') return 'menos específico';
    return type; // Mapeo por defecto
}

function cleanString(str) {
    return str.replace(/[^a-zA-Z0-9]/g, '');
}

let cy = cytoscape({
    container: document.getElementById('cy'),
    elements: [],  // Inicialmente grafo vacío
    style: [
        {
            selector: 'node', // NODOS
            style: {
                'background-color': (node) => { // Colores por terminologia
                    const colorMap = {
                        'chapterciap1': '#b6630b',
                        'ciap1': '#e97d07',
                        'chaptercie10': '#0F5C99',
                        'cie10': '#7caccc',
                        'snomed': '#754591',
                        'levelatc': '#9b1212',
                        'atc': '#e65a5a',
                        'aemps': '#e9c600',
                        'rxnorm': '#41ac58'
                    };
                    const vocab = node.data('vocab') ? node.data('vocab').toLowerCase() : '';
                    const level = node.data('level');
                    if (vocab === 'chaptercie10' && level === 3) {
                        return '#187bcc'; // secciones
                    }
                    return colorMap[vocab] || '#e054fc';
                },
                'label': (node) => { // Limitamos longitud label+code
                    const fullLabel = node.data('label');
                    const code = node.data('code');
                    const truncatedLabel = fullLabel.length > 90 ? fullLabel.slice(0, 90) + '...' : fullLabel;
                    return `${truncatedLabel}\n(${code})`;
                },
                'color': '#fff',
                'font-family': 'Montserrat, sans-serif',
                'text-valign': 'center',
                'text-halign': 'center',
                'font-size': '7px',
                'border-width': 1,
                'border-color': '#333',
                'padding': '25px',
                'text-wrap': 'wrap',
                'text-max-width': '40px'
            }
        },
        {
            selector: 'edge', // EJES
            style: {
                'width': 2,
                'line-color': '#ccc',
                'font-family': 'Montserrat, sans-serif',
                'target-arrow-color': '#ccc',
                'target-arrow-shape': 'triangle',
                'curve-style': 'bezier',
                'label': (edge) => {  // Mapeo nomenclatura correspondencias
                    return relationshipMapping[edge.data('label')] || edge.data('label');
                },
                'font-size': '8px',
                'text-rotation': 'autorotate', 
                'color': '#000',
                'control-point-step-size': 200
            }
        },
        {
            selector: '.show-info', // NODO AL HACER CLICK
            style: {
                'label': 'data(info)',
                'font-family': 'Montserrat, sans-serif',
                'text-valign': 'center',
                'text-halign': 'center',
                'font-size': '6px',
                'text-max-width': '15px',
                'text-margin-y': '2px',
                'color': '#fff',
                'overlay-padding': 6,
                'z-index': 10,
            }
        }
    ],
    layout: {
        name: 'preset',
        directed: true,
        orientation: 'TB',
        padding: 10,
        spacingFactor: 1.5,
        fit: true,
        avoidOverlap: true
    }
});

// Actualizar el grafo
function updateGraph(data) {
    //cy.elements().remove(); // dejar busqueda
    cy.add(data.nodes);
    cy.add(data.edges);
    organizeGraphByLevels();
}

// Mostrar/ocultar relaciones del nodo al hacer UN CLICK en él------------------------------------------------------------------
cy.on('tap', 'node', function(evt) {
    const node = evt.target;
    const level = node.data('level');
    const vocab = node.data('vocab');
    const inRelations = node.data('in_relations');
    const outRelations = node.data('out_relations');

    // Construir el texto de relaciones
    let relationsText = '';
    if (inRelations && Object.keys(inRelations).length > 0) {
        for (const [type, targets] of Object.entries(inRelations)) {
            const mappedType = relationshipMapping[type] || type;
            if (cleanString(mappedType) === vocab) continue; // excluimos ejes entrantes de mapeos a otras terminologias
            const translatedType = mapRelation(mappedType, 'in', level);
            relationsText += `${translatedType}(${targets.length})\n`;
        }
    }
    if (outRelations && Object.keys(outRelations).length > 0) {
        for (const [type, targets] of Object.entries(outRelations)) {
            const mappedType = relationshipMapping[type] || type;
            const translatedType = mapRelation(mappedType, 'out', level);
            relationsText += `${translatedType}(${targets.length})\n`;
        }
    }

    const infoText = relationsText || 'No hay relaciones disponibles.';

    if (node.hasClass('show-info')) {
        node.removeClass('show-info');
    } else {
        node.data('info', infoText); // Mostrar relaciones
        node.addClass('show-info');
    }
}); //----------------------------------------------------------------------------------------------------------------------------

// Expandir las relaciones de un nodo al hacer DOBLE CLICK en él------------------------------------------------------------------
cy.on('dblclick', 'node', function (evt) {
    const node = evt.target;
    const nodeId = node.id();
    const nodeLevel = node.data('level');
    const nodeVocab = node.data('vocab');
    const inRelations = node.data('in_relations') || {};
    const outRelations = node.data('out_relations') || {};
    // Guardar el zoom y la posición actual
    const currentZoom = cy.zoom();
    const currentPan = cy.pan();

    // construimos checkboxes con las relaciones
    let relationCheckbox = '';
    if (Object.keys(inRelations).length > 0) {
        for (const [type, targets] of Object.entries(inRelations)) {
            const mappedType = relationshipMapping[type] || type;
            if (cleanString(mappedType) === nodeVocab) continue;
            const translatedType = mapRelation(mappedType, 'in', nodeLevel);
            const count = targets.length;
            relationCheckbox += `
                <div style="text-align: left; margin-left: 20px;">
                    <input type="checkbox" class="custom-checkbox" id="relation-in-${type}" value="${type}_in" />
                    <label for="relation-in-${type}" style="margin-left: 8px;">${translatedType}: ${count}</label>
                </div>
            `;
        }
    }
    if (Object.keys(outRelations).length > 0) {
        for (const [type, targets] of Object.entries(outRelations)) {
            const mappedType = relationshipMapping[type] || type;
            const translatedType = mapRelation(mappedType, 'out', nodeLevel);
            const count = targets.length;
            relationCheckbox += `
                <div style="text-align: left; margin-left: 20px;">
                    <input type="checkbox" class="custom-checkbox" id="relation-out-${type}" value="${type}_out" />
                    <label for="relation-out-${type}" style="margin-left: 8px;">${translatedType}: ${count}</label>
                </div>
            `;
        }
    }
    if (!relationCheckbox) {
        Swal.fire({
            title: 'Sin relaciones',
            text: 'Este nodo no tiene relaciones disponibles para expandir.',
            icon: 'info',
            confirmButtonText: 'Aceptar',
        });
        return;
    }

    // montamos pop-up
    Swal.fire({
        title: 'Correspondencias a expandir',
        html: `
            <div style="text-align: left; max-height: 300px; overflow-y: auto; padding: 10px;">
                ${relationCheckbox}
            </div>
        `,
        customClass: {
            title: 'custom-title-class'
        },
        showCancelButton: true,
        confirmButtonText: 'Expandir',
        cancelButtonText: 'Cancelar',
        confirmButtonColor: '#0F5C99',
        cancelButtonColor: '#6d6d6d',
        width: '25%',
        preConfirm: () => {
            const selectedEdgeIds = [];
            Object.keys(inRelations).forEach(type => {
                const checkbox = document.getElementById(`relation-in-${type}`);
                if (checkbox && checkbox.checked) {
                    inRelations[type].forEach(edgeId => selectedEdgeIds.push(edgeId));
                }
            });
            Object.keys(outRelations).forEach(type => {
                const checkbox = document.getElementById(`relation-out-${type}`);
                if (checkbox && checkbox.checked) {
                    outRelations[type].forEach(edgeId => selectedEdgeIds.push(edgeId));
                }
            });
            return selectedEdgeIds;
        },
    }).then((result) => {
        if (result.isConfirmed) {
            const selectedEdgeIds = result.value; 
            document.getElementById('loading-spinner').classList.remove('hidden');
            const edgeIdsParam = selectedEdgeIds.map(id => encodeURIComponent(id)).join(',');

            // Solicitud al servidor con los tipos de relaciones seleccionadas
            fetch(`/api/expand?node_id=${encodeURIComponent(nodeId)}&node_level=${encodeURIComponent(nodeLevel)}&edge_ids=${edgeIdsParam}`)
            .then((response) => response.json())
                .then((data) => {
                    if (data.nodes.length > 0) {
                        cy.add(data.nodes);
                        cy.add(data.edges);
                        organizeGraphByLevels();
                        node.removeClass('show-info');
                        cy.zoom(currentZoom);
                        cy.pan(currentPan);
                    } else {
                        Swal.fire({
                            title: 'Sin diagnósticos relacionados',
                            text: 'No se encontraron diagnósticos relacionados para las correspondencias seleccionadas.',
                            icon: 'info',
                            confirmButtonText: 'Aceptar',
                        });
                    }
                })
                .catch((error) => {
                    console.error('Error al expandir el nodo:', error);
                    Swal.fire({
                        title: 'Error',
                        text: 'Ocurrió un error al intentar expandir las correspondencias del diagnóstico.',
                        icon: 'error',
                        confirmButtonText: 'Aceptar',
                    });
                })
                .finally(() => {
                    document.getElementById('loading-spinner').classList.add('hidden');
                });
        }
    });
}); //----------------------------------------------------------------------------------------------------------------------------

// Info nodo al hacer CLICK DERECHO en el ----------------------------------------------------------------------------------------
cy.on('cxttap', 'node', function(evt) {
    const node = evt.target;
    const nodeLabel = node.data('label') || 'Sin etiqueta';
    const nodeCode = node.data('code') || 'Sin código';
    const nodeClass = node.data('vocab') || 'Sin terminología';
    //const nodeLevel = node.data('level') || 'Nivel no especificado';
    const nodeId = node.id();

    Swal.fire({
        title: 'Información del diagnóstico',
        html: `
            <div style="text-align: left; margin-left: 35px; margin-right: 35px; line-height: 1.5; font-size: 15px;">
                <p><strong>Diagnóstico:</strong> ${nodeLabel}</p>
                <p><strong>Código:</strong> ${nodeCode}</p>
                <p><strong>Terminología:</strong> ${nodeClass}</p>
            </div>
        `,
        customClass: {
            title: 'custom-title-class'
        },
        confirmButtonText: 'Aceptar',
        confirmButtonColor: '#6d6d6d',  
        width: '25%'
    });
}); //----------------------------------------------------------------------------------------------------------------------------

// Goma de borrar nodos ----------------------------------------------------------------------------------------------------------
const eraseButton = document.getElementById('erase-button');

// Controlar el modo de borrado
let eraseMode = false;

// Alternar el modo de borrado al hacer clic en el botón
eraseButton.addEventListener('click', function () {
    eraseMode = !eraseMode;
    eraseButton.style.backgroundColor = eraseMode ? '#d33' : '#0F5C99';

    if (eraseMode) {
        Swal.fire({
            icon: 'info',
            title: 'Modo borrar activado',
            text: 'Haz clic en los elementos que deseas eliminar.',
            confirmButtonColor: '#6d6d6d',
            confirmButtonText: 'Aceptar',
        });
    }
});

// Eliminar nodos cuando el modo borrar está activo
cy.on('tap', 'node', function (evt) {
    if (eraseMode) {
        const node = evt.target;
        const connectedEdges = node.connectedEdges();
        const elementsToRemove = cy.collection();
        elementsToRemove.merge(connectedEdges);
        elementsToRemove.merge(node);
        cy.remove(elementsToRemove);
    }
}); //----------------------------------------------------------------------------------------------------------------------------

// Exportar a CSV ----------------------------------------------------------------------------------------------------------------
document.getElementById('export-button').addEventListener('click', function () {
    const visibleNodes = cy.nodes(':visible').map(node => node.data());

    if (visibleNodes.length === 0) {
        Swal.fire({
            icon: 'warning',
            title: 'No hay nada para exportar',
            text: 'Por favor, asegúrate de que los elementos estén visibles antes de intentar exportarlos.',
            confirmButtonColor: '#6d6d6d',
            confirmButtonText: 'Aceptar',
        });
        return;
    }

    // Mostrar el pop-up
    Swal.fire({
        title: 'Exportar',
        html: `
            <p>Estás a punto de exportar a CSV los <strong>${visibleNodes.length}</strong> elementos visibles.</p>
            <p>¿Deseas continuar?</p>
        `,
        icon: 'info',
        showCancelButton: true,
        confirmButtonText: 'Exportar',
        cancelButtonText: 'Cancelar',
        confirmButtonColor: '#0F5C99',
        cancelButtonColor: '#6d6d6d',
    }).then(result => {
        if (result.isConfirmed) {
            const headers = ['Diagnóstico', 'Código', 'Terminología'];
            const sortedNodes = visibleNodes.sort((a, b) => a.code.localeCompare(b.code, 'es', { numeric: true }));
            const rows = sortedNodes.map(node => [
                node.label.replace(/\|/g, ','), // Reemplazar sinónimos en etiquetas
                node.code,
                node.vocab
            ]);

            const csvContent = [headers.join(';'), ...rows.map(row => row.join(';'))].join('\n');

            // Crear un archivo blob y descargarlo
            const blob = new Blob([`\ufeff${csvContent}`], { type: 'text/csv;charset=utf-8;' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'correspondencias.csv';
            a.style.display = 'none';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
        }
    });
}); // ---------------------------------------------------------------------------------------------------------------------------


// Organizar el grafo por niveles-------------------------------------------------------------------------------------------------
function organizeGraphByLevels() {
    const levelSpacing = 150; // Vertical
    const horizontalSpacing = 150; // Horizontal
    const processedNodes = new Set(); // Set para evitar repetir nodo

    const levelMap = {}; // Agrupamos nodos por nivel y ordenamos niveles. Ordenar nodos dentro del nivel?
    cy.nodes().forEach(node => {
        const level = node.data('level');
        if (!levelMap[level]) {
            levelMap[level] = [];
        }
        levelMap[level].push(node);
    });
    const levels = Object.keys(levelMap).map(Number).sort((a, b) => a - b);

    // Asignamos posiciones a los nodos empezando desde el nivel más bajo
    levels.forEach(level => {
        const nodesInLevel = levelMap[level];

        nodesInLevel.forEach(node => {
            if (processedNodes.has(node.id())) {
                return; // Saltamos nodos ya procesados
            }
            const parentEdges = node.connectedEdges().filter(edge => {
                const isIsaOrSynonym = edge.data('label') === 'Isa' || edge.data('label') === 'synonym';
                if (isIsaOrSynonym) {
                    return edge.source().id() === node.id(); // Relación Isa
                } else {
                    return edge.target().id() === node.id(); // Otras
                }
            });
            if (parentEdges.length > 0) { // Caso 1: Centrar los nodos hijos respecto al padre
                let parentNode;
                if (parentEdges[0].source().id() === node.id()) {
                    parentNode = parentEdges[0].target(); // Relación Isa o synonym
                } else {
                    parentNode = parentEdges[0].source();
                }
                const parentPosition = parentNode.position();
                const siblingEdges = parentNode.connectedEdges().filter(edge => {
                    const isIsaOrSynonym = edge.data('label') === 'Isa' || edge.data('label') === 'synonym';
                    if (isIsaOrSynonym) {
                        return edge.target().id() === parentNode.id(); // Relación Isa o synonym
                    } else {
                        return edge.source().id() === parentNode.id(); 
                    }
                });
                const siblings = siblingEdges.map(edge => {
                    let siblingNode;
                    const isIsaOrSynonym = edge.data('label') === 'Isa' || edge.data('label') === 'synonym';
                    if (isIsaOrSynonym) {
                        siblingNode = edge.source(); // Relación Isa o synonym
                    } else {
                        siblingNode = edge.target();
                    }
                    return siblingNode;
                });
                const siblingCount = siblings.length;
                const totalSiblingWidth = (siblingCount - 1) * horizontalSpacing;
                const offsetX = parentPosition.x - totalSiblingWidth / 2; 
                const siblingIndex = siblings.findIndex(sibling => sibling.id() === node.id());
                const xPosition = offsetX + siblingIndex * horizontalSpacing;
                const yPosition = parentPosition.y + levelSpacing;
                node.position({ x: xPosition, y: yPosition });

            } else { // Caso 2: Nodo raíz (no tiene padre)
                const indexInLevel = levelMap[level].indexOf(node);
                let xPosition;
                if (level === 1 && nodesInLevel.length === 2) {
                    xPosition = indexInLevel * horizontalSpacing*40; // Espaciado especial para nivel 1
                } else {
                    xPosition = indexInLevel * horizontalSpacing; // Espaciado normal
                }const yPosition = level * levelSpacing;
                node.position({ x: xPosition, y: yPosition });
            }
            processedNodes.add(node.id());
        });
    });

    cy.fit(cy.elements(), 50);
} // ------------------------------------------------------------------------------------------------------------------------------

// Controlar el zoom
cy.on('zoom', function() {
    const currentZoom = cy.zoom();
    document.getElementById('zoom-slider').value = currentZoom;
});

document.getElementById('zoom-slider').addEventListener('input', function(event) {
    const zoomLevel = parseFloat(event.target.value);
    cy.zoom(zoomLevel);
    cy.center(); 
});
