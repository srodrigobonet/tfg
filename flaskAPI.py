import os
import requests
from requests.utils import quote
from flask import Flask, jsonify, send_from_directory, request #type:ignore

app = Flask(__name__)

# Configuración de la base de datos
base_url = "http://1.44.4.82:2480"
db_name = "Atlas"
user = "root"
password = "RyC1852!"

APP_ROOT = "/aplicacion"

@app.route('/')
def home():
    return send_from_directory(APP_ROOT, 'index.html')



# Endpoint de búsqueda para obtener nodos y relaciones concretos ----------------------------------------------------------------------------------------------------------------------------------
@app.route('/api/search', methods=['GET'])
def search_graph():
    term = request.args.get('term', '')
    code = request.args.get('code', '')
    vocab = request.args.get('vocab', '')

    # Si ninguno de los parámetros tiene valor
    if not term and not code and not vocab:
        return jsonify({"nodes": [], "edges": []})

    try:
        # 1. Obtenemos nombre de todas las relaciones
        relations_query = """
            SELECT name 
            FROM (SELECT expand(classes) FROM metadata:schema) 
            WHERE superClass = 'E'
        """
        relations_url = f"{base_url}/query/{db_name}/sql/{quote(relations_query)}"
        relations_response = requests.get(relations_url, auth=(user, password))
        relations_response.raise_for_status()
        relations = [rel['name'] for rel in relations_response.json().get('result', [])]

        # 2. Obtenemos nodos (y relaciones de cada nodo)
        select_fields = [
            "@rid AS id",
            "concept_name AS label",
            "IFNULL(concept_name_spanish, '') AS label2",
            "IFNULL(synonymous_concept, '') AS label3",
            "concept_code AS code",
            "@class AS vocab"
        ]
        for relation in relations:
            select_fields.append(f"in_{relation} AS in_{relation}")
            select_fields.append(f"out_{relation} AS out_{relation}")
        
        node_query = f"SELECT {', '.join(select_fields)} FROM V"
        filters = []
        if term:
            filters.append(f"(concept_name LIKE '%{term}%' OR concept_name_spanish LIKE '%{term}%' OR synonymous_concept LIKE '%{term}%')")
        if code:
            filters.append(f"concept_code LIKE '%{code}%'")
        if vocab:
            vocab_list = [v.strip() for v in vocab.split(',')] 
            filters.append(f"@class IN {vocab_list}")
        if filters:
            node_query += " WHERE " + " AND ".join(filters)
        node_query += " LIMIT -1"

        nodes_url = f"{base_url}/query/{db_name}/sql/{node_query}"
        nodes_response = requests.get(nodes_url, auth=(user, password))
        nodes_response.raise_for_status()
        nodes_result = nodes_response.json().get('result', [])

        nodes = []
        for node in nodes_result:
            # tratamos label
            label_spanish = node.get('label2')  # concept_name_spanish
            label_synonym = node.get('label3')  # synonymous_concept
            label_english = node.get('label')   # concept_name
            label = (
                label_spanish if label_spanish and label_spanish != '[]' else
                label_synonym if label_synonym and label_synonym != '[]' else
                label_english or ''
            )
            # tratamos relaciones
            in_relations = {key.replace("in_", ""): value for key, value in node.items() if key.startswith("in_") and value}
            out_relations = {key.replace("out_", ""): value for key, value in node.items() if key.startswith("out_") and value}

            node_data = {
                "data": {
                    "id": str(node['id']),
                    "label": (label or "").capitalize(),
                    "code": node['code'],
                    "vocab": node['vocab'],
                    "level": calculate_node_level(node, None),
                    "in_relations": in_relations, 
                    "out_relations": out_relations
                }
            }
            nodes.append(node_data)

        # 3. Obtenemos relaciones entre esos nodos
        node_ids = [f"'{str(node['id'])}'" for node in nodes_result]

        if node_ids:
            edges_query = f"SELECT @rid AS id, out AS source, in AS target, @class AS label FROM E WHERE out IN [{', '.join(node_ids)}] AND in IN [{', '.join(node_ids)}] LIMIT -1"
            try:
                encoded_edges_query = quote(edges_query) # Formato correcto
                edges_url = f"{base_url}/query/{db_name}/sql/{encoded_edges_query}"
                edges_response = requests.get(edges_url, auth=(user, password))
                edges_response.raise_for_status()
                edges_result = edges_response.json().get('result', [])
                edges = [{"data": {"id": str(rel['id']), "source": str(rel['source']), "target": str(rel['target']), "label": rel['label']}} for rel in edges_result]
            
            except requests.exceptions.HTTPError as e:
                print(f"Error en la obtención de relaciones: {e}")
                edges = []
        else:
            edges = []

        return jsonify({"nodes": nodes, "edges": edges})

    except requests.exceptions.HTTPError as http_err:
        return jsonify({"error": str(http_err)}), 500

    except Exception as e:
        return jsonify({"error": str(e)}), 500
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
# Endpoint de expansion de relaciones de un nodo concreto -----------------------------------------------------------------------------------------------------------------------------------------
@app.route('/api/expand', methods=['GET'])
def expand_node():
    node_id = request.args.get('node_id')
    node_level = request.args.get('node_level', type=int)
    edge_ids = request.args.get('edge_ids', '')

    if not node_id or not edge_ids:
        return jsonify({"nodes": [], "edges": []})

    try:
        # 1. Obtenemos nombre de todas las relaciones
        relations_query = """
            SELECT name 
            FROM (SELECT expand(classes) FROM metadata:schema) 
            WHERE superClass = 'E'
        """
        relations_url = f"{base_url}/query/{db_name}/sql/{quote(relations_query)}"
        relations_response = requests.get(relations_url, auth=(user, password))
        relations_response.raise_for_status()
        relations = [rel['name'] for rel in relations_response.json().get('result', [])]

        # 2. Obtenemos nodos relacionados (y relaciones de cada nodo)
        edge_ids_condition = ",".join([f"'{edge_id.strip()}'" for edge_id in edge_ids.split(',')])
        select_fields = [
            "@rid AS id",
            "concept_name AS label",
            "IFNULL(concept_name_spanish, '') AS label2",
            "IFNULL(synonymous_concept, '') AS label3",
            "concept_code AS code",
            "@class AS vocab"
        ]
        for relation in relations:
            select_fields.append(f"in_{relation} AS in_{relation}")
            select_fields.append(f"out_{relation} AS out_{relation}")

        node_query = f"""
            SELECT {', '.join(select_fields)} FROM V 
            WHERE @rid IN (SELECT DISTINCT expand(bothV()) FROM E WHERE @rid IN [{edge_ids_condition}]) LIMIT -1
        """
        nodes_url = f"{base_url}/query/{db_name}/sql/{quote(node_query)}"
        nodes_response = requests.get(nodes_url, auth=(user, password))
        nodes_response.raise_for_status()
        nodes_result = nodes_response.json().get('result', [])

        nodes = []
        for node in nodes_result:
            # tratamos label
            label_spanish = node.get('label2')  # concept_name_spanish
            label_synonym = node.get('label3')  # synonymous_concept
            label_english = node.get('label')   # concept_name
            label = (
                label_spanish if label_spanish and label_spanish != '[]' else
                label_synonym if label_synonym and label_synonym != '[]' else
                label_english or ''
            )
            # tratamos relaciones
            in_relations = {key.replace("in_", ""): value for key, value in node.items() if key.startswith("in_") and value}
            out_relations = {key.replace("out_", ""): value for key, value in node.items() if key.startswith("out_") and value}

            node_data = {
                "data": {
                    "id": str(node['id']),
                    "label": (label or "").capitalize(),
                    "code": node['code'],
                    "vocab": node['vocab'],
                    "level": calculate_node_level(node, node_level),
                    "in_relations": in_relations,
                    "out_relations": out_relations
                }
            }
            nodes.append(node_data)

        # 3. Añadir relaciones seleccionadas
        edges_query = f"""
        SELECT @rid AS id, out AS source, in AS target, @class AS label 
        FROM E 
        WHERE @rid IN [{edge_ids_condition}] LIMIT -1
        """
        edges_url = f"{base_url}/query/{db_name}/sql/{quote(edges_query)}"
        edges_response = requests.get(edges_url, auth=(user, password))
        edges_response.raise_for_status()
        edges_result = edges_response.json().get('result', [])
        edges = [{"data": {"id": str(edge['id']), "source": str(edge['source']), "target": str(edge['target']), "label": edge['label']}} for edge in edges_result]

        return jsonify({"nodes": nodes, "edges": edges})

    except requests.exceptions.HTTPError as http_err:
        return jsonify({"error": str(http_err)}), 500
    except Exception as e:
        return jsonify({"error": str(e)}), 500
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    

# Función para calcular el nivel del nodo ---------------------------------------------------------------------------------------------------------------------------------------------------------
def calculate_node_level(node, expanded_node_level=None):
    root_ids = ['#67:4', '#47:78']
    node_class = node.get('vocab', None)
    class_hierarchy = {
        "ChapterCIAP1": 2,
        "CIAP1": 3,
        "ChapterCIE10": None,  
        "CIE10": None,
        "SNOMED": None, 
    }

    # Nodos raíz ChapterCIAP1, ChapterCIE10: nivel 1
    if node['id'] in root_ids:
        return 1

    # ChapterCIAP1, CIAP1: nivel 2 y 3
    if node_class in class_hierarchy:
        if class_hierarchy[node_class] is not None:
            return class_hierarchy[node_class]
        
    # ChapterCIE10,CIE10: distancia al nodo raíz + 1
    if node_class in ['ChapterCIE10', 'CIE10']:
        node_id = node['id']
        distances = []
        distance_query = f"""
            SELECT shortestPath({node_id}, {root_ids[1]}, 'BOTH', 
                E[@class IN ['include', 'Isa', 'synonym']]) AS path 
            FROM V 
            WHERE @class IN ['ChapterCIE10', 'CIE10']
        """

        try:
            path_url = f"{base_url}/query/{db_name}/sql/{quote(distance_query)}"
            response = requests.get(path_url, auth=(user, password))
            response.raise_for_status()
            path_result = response.json().get('result', [])
            if path_result and isinstance(path_result[0].get('path'), list):
                distances.append(len(path_result[0]['path']) - 1)
        except Exception as e:
            print(f"Error shortest path: {e}")

        if distances:
            return min(distances) + 1 

    # SNOMED: un nivel mas que el expandido o 4 por defecto
    if node_class == 'SNOMED':
        if expanded_node_level is not None:
            return expanded_node_level + 1
        else:
            return 4
        
    # LevelATC, ATC: nivel basado en la longitud del código (igual con cie10 tambien)
    if node_class in ['LevelATC', 'ATC']:
        node_code = node.get('code', '')
        if isinstance(node_code, str):
            alphanumeric_characters = ''.join(filter(str.isalnum, node_code))
            return len(alphanumeric_characters)
        else:
            print(f"Advertencia: Nodo {node['id']} no tiene un código válido.")
            return -1

    return -1 
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Rutas para servir archivos estáticos de las carpetas css y js
@app.route('/css/<path:filename>')
def serve_css(filename):
    return send_from_directory(os.path.join(APP_ROOT, 'css'), filename)

@app.route('/js/<path:filename>')
def serve_js(filename):
    return send_from_directory(os.path.join(APP_ROOT, 'js'), filename)

if __name__ == '__main__':
    app.run(host="0.0.0.0", debug=True)

