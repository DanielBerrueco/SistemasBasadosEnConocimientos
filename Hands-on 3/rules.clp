; =====================================================
; Funciones de ayuda
; =====================================================

(deffunction obtener-precio (?sku)
   "Devuelve el precio unitario del producto con el SKU dado, buscando en smartphone, Computador y Accesorios."
   (if (neq (length$ (find-all-facts ((?s smartphone)) (eq ?s:sku-smartphone ?sku))) 0)
      then (fact-slot-value (nth$ 1 (find-all-facts ((?s smartphone)) (eq ?s:sku-smartphone ?sku))) precio)
      else
         (if (neq (length$ (find-all-facts ((?c Computador)) (eq ?c:sku-computador ?sku))) 0)
            then (fact-slot-value (nth$ 1 (find-all-facts ((?c Computador)) (eq ?c:sku-computador ?sku))) precio)
            else
               (if (neq (length$ (find-all-facts ((?a Accesorios)) (eq ?a:sku-accesorio ?sku))) 0)
                  then (fact-slot-value (nth$ 1 (find-all-facts ((?a Accesorios)) (eq ?a:sku-accesorio ?sku))) precio)
                  else 0
               )
         )
   )
)

; Función que recorre todos los ItemOrden asociados a una OrdenCompra y retorna el total.
(deffunction calcular-total-orden (?oid)
   (bind ?sum 0)
   (do-for-all-facts ((?item ItemOrden))
      (eq (fact-slot-value ?item orden-id) ?oid)
      (bind ?sum (+ ?sum (* (fact-slot-value ?item cantidad)
                            (obtener-precio (fact-slot-value ?item producto-sku)))))
   )
   ?sum
)

; =====================================================
; Reglas
; =====================================================

;; 1. Promo: iPhone16 con Banamex → 24 meses sin intereses
(defrule promo-iphone16-banamex
   (declare (salience 200))
   (OrdenCompra (orden-id ?oid) (cliente-id ?cid))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku) (procesado no))
   (smartphone (sku-smartphone ?sku) (modelo iPhone16))
   (TarjetaCredito (cliente-id ?cid) (banco "Banamex"))
   =>
   (printout t "PROMOCIÓN: iPhone 16 con tarjeta Banamex - 24 meses sin intereses." crlf)
)


;; 2. Promo: Samsung Note 21 con tarjeta Liverpool VISA → 12 meses sin intereses
(defrule promo-note21-liverpool
   (OrdenCompra (orden-id ?oid) (cliente-id ?cid))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku))
   (smartphone (sku-smartphone ?sku) (modelo SamsungNote21))
   (TarjetaCredito (cliente-id ?cid) (banco "Liverpool") (grupo "Visa"))
   =>
   (printout t "PROMOCIÓN: Samsung Note 21 con tarjeta Liverpool VISA - 12 meses sin intereses." crlf)
)

;; 3. Promo: MacBook Air + iPhone 16 al contado → vales
(defrule promo-macbook-iphone-contado
   ?order <- (OrdenCompra (orden-id ?oid) (cliente-id ?cid) (procesada no))
   (ItemOrden (orden-id ?oid) (producto-sku ?skuMac) (cantidad ?c1))
   (Computador (sku-computador ?skuMac) (modelo MacBookAir) (precio ?p1))
   (ItemOrden (orden-id ?oid) (producto-sku ?skuPh) (cantidad ?c2))
   (smartphone (sku-smartphone ?skuPh) (modelo iPhone16) (precio ?p2))
   ?client <- (Clientes (cliente-id ?cid) (puntos ?pts))
   =>
   (bind ?subtotal (+ (* ?p1 ?c1) (* ?p2 ?c2)))
   (bind ?num (/ ?subtotal 1000))
   (bind ?voucher (* ?num 100))
   (modify ?client (puntos (+ ?pts ?voucher)))
   (assert (Vale (vale-id 0) (valor ?voucher)))
   (modify ?order (procesada si))
   (printout t "BONO: Compra al contado de MacBook Air + iPhone16. Se otorgan " ?voucher 
             " pesos en vales y se suman a los puntos del cliente." crlf)
)


;; 4. Promo: Smartphone → Ofrecer funda y mica con 15% de descuento
(defrule promo-smartphone-accesorios
   (OrdenCompra (orden-id ?oid) (procesada no))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku))
   (smartphone (sku-smartphone ?sku))
   =>
   (printout t "OFERTA: Funda y mica con 15% de descuento en accesorios por compra de smartphone." crlf)
)

;; 5. Cancelar registro si tarjeta vencida
(defrule validar-expiracion-tarjeta
   ?t <- (TarjetaCredito (cliente-id ?cid) (fecha-expiracion ?month ?year))
   (test (or (< ?year 2025) (and (= ?year 2025) (< ?month 5))))
   =>
   (printout t "ERROR: Tarjeta vencida. Registro cancelado para cliente " ?cid crlf)
   (retract ?t)
)

;; 6. Orden mayoreo → Si algún ItemOrden tiene cantidad mayor a 10, se aplica descuento.
(defrule orden-mayoreo-descuento
   (declare (salience 50))
   (OrdenCompra (orden-id ?oid) (total ?t) (procesada no))
   (ItemOrden (orden-id ?oid) (cantidad ?c&:(> ?c 10)))
   =>
   (bind ?descuento (* ?t 0.10))
   (bind ?nuevo-total (- ?t ?descuento))
   (printout t "ORDEN MAYOREO: Se aplica 10% de descuento. Total original: $" ?t ", con descuento: $" ?nuevo-total crlf)
)

;; 7a. Verificar stock insuficiente - Smartphone
(defrule verificar-stock-smartphone
   (declare (salience 100))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?cant) (procesado no))
   ?s     <- (smartphone (sku-smartphone ?sku) (stock ?stock))
   (test (< ?stock ?cant))
   =>
   (printout t "ERROR: Stock insuficiente para smartphone. Orden " ?oid " cancelada." crlf)
   (retract ?order)
)

;; 7b. Verificar stock insuficiente - Computador
(defrule verificar-stock-computador
   (declare (salience 100))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?cant) (procesado no))
   ?comp  <- (Computador (sku-computador ?sku) (stock ?stock))
   (test (< ?stock ?cant))
   =>
   (printout t "ERROR: Stock insuficiente para Computador. Orden " ?oid " cancelada." crlf)
   (retract ?order)
)

;; 7c. Verificar stock insuficiente - Accesorio
(defrule verificar-stock-accesorio
   (declare (salience 100))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?cant) (procesado no))
   ?acc   <- (Accesorios (sku-accesorio ?sku) (stock ?stock))
   (test (< ?stock ?cant))
   =>
   (printout t "ERROR: Stock insuficiente para Accesorio. Orden " ?oid " cancelada." crlf)
   (retract ?order)
)

;; 8a. Actualizar stock - Smartphone
(defrule actualizar-stock-smartphone
   (declare (salience 20))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?c) (procesado no))
   ?s     <- (smartphone (sku-smartphone ?sku) (stock ?stock))
   (test (>= ?stock ?c))
   =>
   (modify ?s (stock (- ?stock ?c)))
   (modify ?item (procesado si))
   (printout t "Stock actualizado para smartphone en Orden " ?oid ": " (- ?stock ?c) crlf)
)

;; 8b. Actualizar stock - Computador
(defrule actualizar-stock-computador
   (declare (salience 20))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?c) (procesado no))
   ?comp  <- (Computador (sku-computador ?sku) (stock ?stock))
   (test (>= ?stock ?c))
   =>
   (modify ?comp (stock (- ?stock ?c)))
   (modify ?item (procesado si))
   (printout t "Stock actualizado para Computador en Orden " ?oid ": " (- ?stock ?c) crlf)
)

;; 8c. Actualizar stock - Accesorio
(defrule actualizar-stock-accesorio
   (declare (salience 20))
   ?order <- (OrdenCompra (orden-id ?oid) (procesada no))
   ?item  <- (ItemOrden (orden-id ?oid) (producto-sku ?sku) (cantidad ?c) (procesado no))
   ?acc   <- (Accesorios (sku-accesorio ?sku) (stock ?stock))
   (test (>= ?stock ?c))
   =>
   (modify ?acc (stock (- ?stock ?c)))
   (modify ?item (procesado si))
   (printout t "Stock actualizado para Accesorio en Orden " ?oid ": " (- ?stock ?c) crlf)
)

;; 9. Verificar duplicado de cliente-id
(defrule verificar-cliente-duplicado
   ?c1 <- (Clientes (cliente-id ?id) (nombre ?nombre1))
   ?c2 <- (Clientes (cliente-id ?id) (nombre ?nombre2))
   (test (neq ?c1 ?c2))
   =>
   (printout t "ERROR: ID de cliente duplicado. Registro cancelado para cliente " ?id crlf)
   (retract ?c2)
)

;; 10. Producto inexistente
(defrule producto-inexistente
   (OrdenCompra (orden-id ?oid) (procesada no))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku))
   (not (or
         (smartphone (sku-smartphone ?sku))
         (Computador (sku-computador ?sku))
         (Accesorios (sku-accesorio ?sku))
       ))
   =>
   (printout t "ERROR: Producto con SKU " ?sku " no existe. Orden " ?oid " cancelada." crlf)
)

;; 11. Calcular el total de la OrdenCompra
(defrule calcular-total-pedido
   (declare (salience 90))
   ?order <- (OrdenCompra (orden-id ?oid) (total ?t&:(= ?t 0)) (procesada no))
   =>
   (bind ?sum (calcular-total-orden ?oid))
   (modify ?order (total ?sum))
   (printout t "Total calculado para Orden " ?oid ": $" ?sum crlf)
)

;; 12. Ofrecer un descuento si un telefono es negro y cuesta más de 500
(defrule promo-smartphone-negro-alta-gama
   (declare (salience 200))
   ?order <- (OrdenCompra (orden-id ?oid) (cliente-id ?cid) (procesada no))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku) (procesado no))
   (smartphone (sku-smartphone ?sku) (color negro) (precio ?p&:(> ?p 500)))
   =>
   (printout t "PROMO: Smartphone negro de gama alta detectado (SKU " ?sku ") en la Orden " ?oid ". Obtenga un descuento especial en su próxima compra." crlf)
)


;; 13. Si un accesorio es menor a 50 otorga 10 puntos extras al cliente
(defrule bonus-accesorios-economicos
   (OrdenCompra (orden-id ?oid) (cliente-id ?cid))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku) (procesado no))
   (Accesorios (sku-accesorio ?sku) (precio ?p&:(< ?p 50)))
   ?client <- (Clientes (cliente-id ?cid) (puntos ?pts))
   =>
   (modify ?client (puntos (+ ?pts 10)))
   (printout t "BONUS: Se suman 10 puntos extra al cliente " ?cid " por la compra de un accesorio económico." crlf)
)

;; 14. Verifica que el cliente tenga un numero de telefono
(defrule alerta-cliente-sin-telefono
   (Clientes (cliente-id ?cid) (telefono ?tel&:(eq ?tel "")))
   =>
   (printout t "ALERTA: Cliente " ?cid " no tiene teléfono registrado." crlf)
)

;; 15. si un accesorio y una computaodra son se la misma marca notificar de una promocion
(defrule promo-combo-comp-accesorio-misma-marca
   (declare (salience 200))
   (OrdenCompra (orden-id ?oid))
   (ItemOrden (orden-id ?oid) (producto-sku ?skuComp))
   (Computador (sku-computador ?skuComp) (marca ?marca))
   (ItemOrden (orden-id ?oid) (producto-sku ?skuAcc))
   (Accesorios (sku-accesorio ?skuAcc) (marca ?marca))
   =>
   (printout t "PROMO: Combo Computador + Accesorio de la marca " ?marca " activado en la Orden " ?oid "." crlf)
)

;; 16. Hacer restock si algun producto se queda en 0
(defrule autoreponer-stock-accesorios
   ?acc <- (Accesorios (sku-accesorio ?sku) (stock 0))
   =>
   (modify ?acc (stock 10))
   (printout t "AUTO: Reposición automática para Accesorios. SKU " ?sku " repuesto a 10 unidades." crlf)
) 

(defrule autoreponer-stock-smartphone
   ?s <- (smartphone (sku-smartphone ?sku) (stock 0))
   =>
   (modify ?s (stock 10))
   (printout t "AUTO: Reposición automática para Smartphone. SKU " ?sku " repuesto a 10 unidades." crlf)
)

(defrule autoreponer-stock-computador
   ?c <- (Computador (sku-computador ?sku) (stock 0))
   =>
   (modify ?c (stock 10))
   (printout t "AUTO: Reposición automática para Computador. SKU " ?sku " repuesto a 10 unidades." crlf)
)  

;; 17. Si una orden no tiene items asociados notificar
(defrule alerta-orden-sin-items
   (OrdenCompra (orden-id ?oid) (total 0))
   (not (ItemOrden (orden-id ?oid)))
   =>
   (printout t "ALERTA: La Orden " ?oid " no tiene ítems asociados." crlf)
)

;; 18. si una orden tiene dos accesorios del mismo tipo ofrecer un descuento en un tercer accesorio
(defrule promo-dos-accesorios-mismo-tipo
   (OrdenCompra (orden-id ?oid))
   (ItemOrden (orden-id ?oid) (producto-sku ?sku))
   (Accesorios (sku-accesorio ?sku) (tipo ?tipo))
   (test (>= (length$ (find-all-facts ((?a Accesorios))
                     (eq ?a:tipo ?tipo))) 2))
   =>
   (printout t "PROMO: Orden " ?oid ", por comprar dos accesorios del tipo " ?tipo ". Obtenga un descuento especial en un tercer accesorio de este tipo." crlf)
)


;; 19. Si tiene más de 100 puntos otorga un descuento
(defrule promo-fidelidad
   ?client <- (Clientes (cliente-id ?cid) (puntos ?pts&:(>= ?pts 100)))
   =>
   (printout t "PROMO: Cliente " ?cid " es fiel y acumula " ?pts " puntos. Se le otorga un vale de descuento extra." crlf)
   (assert (Vale (vale-id 0) (valor 200)))
)


;; 20. Verificar orden sin cliente registrado
(defrule verificar-orden-sin-cliente
   ?order <- (OrdenCompra (orden-id ?oid) (cliente-id ?cid))
   (not (Clientes (cliente-id ?cid)))
   =>
   (printout t "ERROR: La Orden " ?oid " está asociada a cliente " ?cid ", que no existe en la base de datos." crlf)
   (retract ?order)
)
