(deftemplate smartphone
   (slot sku-smartphone (type INTEGER))
   (slot marca (type SYMBOL))
   (slot modelo)
   (slot precio)
   (slot color)
   (slot stock (type INTEGER) (default 20))
)

(deftemplate Computador
   (slot sku-computador (type INTEGER))
   (slot marca (type SYMBOL))
   (slot modelo)
   (slot precio)
   (slot capacidad)
   (slot ram)
   (slot stock (type INTEGER) (default 20))
)

(deftemplate Accesorios
   (slot sku-accesorio (type INTEGER))
   (slot nombre)
   (slot marca (type SYMBOL))
   (slot modelo)
   (slot precio)
   (slot tipo)
   (slot stock (type INTEGER) (default 20))
)

(deftemplate Clientes
   (multislot nombre)
   (slot cliente-id (type INTEGER))
   (slot direccion)
   (slot telefono)
   (slot puntos (type INTEGER) (default 0))
)

(deftemplate OrdenCompra
   (slot orden-id (type INTEGER))
   (slot cliente-id (type INTEGER))
   (slot fecha)
   (slot total (default 0))
   (slot procesada (default no))
)

(deftemplate ItemOrden
   (slot orden-id (type INTEGER))    ; Relaciona el ítem con la orden
   (slot producto-sku (type INTEGER))
   (slot cantidad (type INTEGER))
   (slot procesado (default no)) ; Indica si el ítem ha sido procesado
)

(deftemplate TarjetaCredito
   (slot cliente-id (type INTEGER))
   (slot tarjeta-id (type INTEGER))
   (slot banco)
   (slot grupo)
   (multislot fecha-expiracion (type INTEGER))
)

(deftemplate Vale
    (slot vale-id (type INTEGER))
    (slot valor (type INTEGER))
)

