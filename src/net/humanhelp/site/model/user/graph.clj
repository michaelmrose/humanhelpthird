(ns net.humanhelp.site.model.user.graph
  "User-specific Graph extensions.

   Ordinary User Graph behavior is derived by gesso.model from
   user.schema/user-descriptor:

   - lookup by User ID;
   - lookup by canonical phone;
   - lookup by canonical email;
   - persisted-field projection.

   This namespace is intentionally not a wrapper around generated resolvers.
   Add code here only when User gains a genuinely derived Graph value or
   relationship that cannot be expressed by the descriptor.")

(def custom-resolvers
  "Hand-written User Graph resolvers.

   User currently has no custom Graph behavior."
  [])
