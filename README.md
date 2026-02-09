#  Sistema Bancario Transaccional (Core Banking Simulator)

![COBOL](https://img.shields.io/badge/Language-COBOL-blue?style=for-the-badge)
![CICS](https://img.shields.io/badge/Environment-CICS-green?style=for-the-badge)
![DB2](https://img.shields.io/badge/Database-DB2-red?style=for-the-badge)

> Un sistema de simulación bancaria "Full Stack Mainframe" diseñado para entornos de misión crítica, enfocado en la **integridad transaccional (ACID)**, **optimización de recursos** y **arquitectura pseudoconversacional**.

---

## Video explicativo (clickea la imagen)

[![SAT](assets%2FImagenVideo.png)](LINK DE YOUTUBE)

---

##  Descripción del Proyecto

Este proyecto simula las operaciones centrales de un Core Bancario. A diferencia de un ejercicio académico simple, este sistema está construido bajo estándares de la industria financiera real, resolviendo problemas de **concurrencia**, **bloqueo de registros** y **navegación eficiente** en grandes volúmenes de datos.

### Funcionalidades Principales:
*  **Seguridad:** Login validado contra DB2 y gestión de sesión segura vía `COMMAREA`.
*  **Caja:** Depósitos y Retiros con validación de saldo en tiempo real.
*  **Transferencias:** Movimiento de fondos entre terceros con garantía de atomicidad (Commit/Rollback).
*  **Historial Inteligente:** Visualización de movimientos con paginación optimizada (Scroll Infinito), filtros dinámicos y ordenamiento.

---

##  Arquitectura y Decisiones Técnicas

Este es el valor central del proyecto. El código no solo "funciona", sino que es **eficiente**.

![DiagramaPseudoconversacional.png.jpg](assets%2FDiagramaPseudoconversacional.png)

### 1. Modelo CICS Pseudoconversacional
Para maximizar la escalabilidad y reducir el consumo de recursos del servidor:
* El sistema **libera la memoria y la tarea** después de cada interacción con el usuario (`RETURN TRANSID`).
* El estado de la sesión y el contexto del usuario se preservan y transmiten a través de la **`COMMAREA`**.
* Se utiliza `EIBCALEN` para determinar el flujo lógico (Primera ejecución vs. Respuesta de usuario).

### 2. Integridad Transaccional (ACID)
En las operaciones monetarias (especialmente Transferencias que afectan a dos cuentas), se implementa un control estricto de **Syncpoints**:
* **Atomicidad:** Se actualizan 4 registros (Saldo origen, Saldo destino, Movimiento origen, Movimiento destino).
* **Lógica de Fallo:** Si *cualquiera* de las operaciones SQL falla (SQLCODE ≠ 0), se ejecuta un **`ROLLBACK`** automático para evitar inconsistencias financieras. Solo si todo es exitoso se hace el **`COMMIT`**.

---

##  Modelo de Datos (DB2 Schema)



![Modelo de Datos](assets%2FDiagramaTablasDB2.png)



---

### 3. Optimización de Base de Datos (DB2 Performance)
Para la consulta del historial, **NO** se traen todos los registros a memoria ni se usan `OFFSETs` costosos:
* **Keyset Pagination:** Se utilizan cursores declarados (`DECLARE CURSOR`) que filtran por ID (`WHERE ID > :LAST_ID`).
* **Scroll:** Lógica de punteros para navegar hacia adelante (`F8`) leyendo solo los registros necesarios (Fetch de 4 en 4).
* **Filtros Dinámicos:** SQL con lógica booleana para filtrar por tipo de operación sin multiplicar la cantidad de cursores.

### 4. Modularidad y Calidad de Código
* **XCTL:** Navegación segura entre programas (Login -> Menú -> Transacción).
* **Copybooks:** Rutinas reutilizables (`CPYVALPD`) para sanitización de inputs numéricos y formateo de mensajes, cumpliendo el principio DRY (Don't Repeat Yourself).


---

##  Estructura del Código

* `PBNKL.cbl` - **Login:** Autenticación y control de acceso.
* `PBNKM.cbl` - **Menú Principal:** Despachador de transacciones.
* `PBNKX.cbl` - **Operaciones de Caja:** Lógica de depósitos/retiros y manejo de bloqueos.
* `PBNKT.cbl` - **Transferencias:** Lógica compleja de actualización multi-tabla.
* `PBNKH.cbl` - **Historial:** Motor de consulta con cursores dinámicos y filtros.
* `/COPY` - **Copybooks:** Definiciones de variables globales y rutinas de validación.
* `/BMS` - **Mapsets:** Definición de pantallas y atributos (Colores, protección).


---

## 👨‍💻 Autor

**Martín Rubio** - *Mainframe Developer*
* [LinkedIn](https://www.linkedin.com/in/martin-oscar-rubio-0a0628355/)

---
*Este proyecto fue desarrollado como parte de un portafolio técnico para demostrar competencias avanzadas en el desarrollo de software para el sector bancario/financiero.*



