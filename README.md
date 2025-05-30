## PROYECTO

Este programa en Racket implementa un servicio web RESTful para realizar operaciones matemáticas sobre polinomios, representados como listas de coeficientes. Utiliza funciones puras y recursivas propias de la programación funcional para sumar, multiplicar y derivar polinomios. El servidor, basado en el módulo web-server/servlet, maneja solicitudes HTTP y responde en formato JSON, facilitando la comunicación con el front-end. En la interfaz web, se emplea Bootstrap para el diseño responsivo y fetch para interactuar dinámicamente con el servidor, logrando así una experiencia eficiente y moderna.

---
### 1. Requerimientos
---

#### FrontEnd

1. Instalar la extensión **Open Live Server** en Visual Studio Code.
2. Abrir el archivo HTML en VS Code.
3. Ejecutar **Open Live Server** para visualizar la interfaz.

#### BackEnd

1. Descargar **DrRacket** desde [https://racket-lang.org](https://racket-lang.org).
2. Instalar según tu sistema operativo.

---
### 2. Ejecución del Proyecto
---

#### 🔙 Backend (Servidor Racket)

1. Abrir **DrRacket**.  
2. Cargar el archivo `servidor.rkt`.  
3. Hacer clic en **Run** para iniciar el servidor (escuchará en `http://localhost:8000/api`).  

#### 🌐 Frontend (Cliente Web)

1. Abrir el archivo `index.html` en **Visual Studio Code**.  
2. Hacer clic derecho sobre el archivo y seleccionar **Open with Live Server**.
3. El navegador se abrirá automáticamente mostrando la interfaz para interactuar con el servidor.

--- 

![Vista previa del proyecto](/screenShot/vistaPrevia.png)

---


