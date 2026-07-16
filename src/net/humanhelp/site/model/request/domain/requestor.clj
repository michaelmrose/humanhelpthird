(ns net.humanhelp.site.model.request.domain.requestor
  "Pure Request-owned requestor references and control checks.

   A requestor is either:

   - a known User identity; or
   - a Request-owned capability used by an anonymous customer.

   This namespace only validates the structural reference. It does not prove
   that a User exists, that a capability token is valid, or that the current
   session controls either identity. Those facts must be established by the
   appropriate model or request boundary before calling these predicates.")

(def requestor-types
  #{:user
    :capability})

(defn requestor-type?
  [value]
  (contains?
   requestor-types
   value))

(defn requestor-reference?
  [value]
  (and
   (map? value)
   (=
    #{:requestor/type
      :requestor/id}
    (set
     (keys value)))
   (requestor-type?
    (:requestor/type value))
   (uuid?
    (:requestor/id value))))

(defn user-requestor
  [user-id]
  {:requestor/type
   :user

   :requestor/id
   user-id})

(defn capability-requestor
  [capability-id]
  {:requestor/type
   :capability

   :requestor/id
   capability-id})

(defn user-requestor?
  [value]
  (and
   (requestor-reference?
    value)
   (=
    :user
    (:requestor/type value))))

(defn capability-requestor?
  [value]
  (and
   (requestor-reference?
    value)
   (=
    :capability
    (:requestor/type value))))

(defn requestor
  "Returns the structural requestor reference stored on a Request document.

   The returned value may still be invalid; use requestor-reference? or
   requestor-consistent? when validation is required."
  [request]
  {:requestor/type
   (:request/requestor-type request)

   :requestor/id
   (:request/requestor-id request)})

(defn requestor-type
  [request]
  (:request/requestor-type request))

(defn requestor-id
  [request]
  (:request/requestor-id request))

(defn requestor-consistent?
  [request]
  (requestor-reference?
   (requestor request)))

(defn requested-by-user?
  [request user-id]
  (and
   (uuid? user-id)
   (=
    (user-requestor user-id)
    (requestor request))))

(defn requested-by-capability?
  [request capability-id]
  (and
   (uuid? capability-id)
   (=
    (capability-requestor capability-id)
    (requestor request))))

(defn requested-by?
  [request requestor-reference]
  (and
   (requestor-reference?
    requestor-reference)
   (=
    requestor-reference
    (requestor request))))

(defn controlled-by?
  "Returns true when one of the supplied identities matches the Request's
   stored requestor.

   This is a pure equality check. The caller must first establish that the
   supplied User identity belongs to the current session or that the supplied
   capability has been authenticated."
  [request
   {:keys
    [user-id
     capability-id]}]
  (or
   (requested-by-user?
    request
    user-id)
   (requested-by-capability?
    request
    capability-id)))
