/**PolicyRule holds information that describes a policy rule, but does not contain information
 about who the rule applies to or which namespace the rule applies to.*/
::orch "kubernetes" "PolicyRule" as PolicyRule @apiGroups, @verbs, @nonResourceURLs, @resources, @resourceNames {
     /**APIGroups is the name of the APIGroup that contains the resources.  If multiple API groups
      are specified, any action requested against one of the enumerated resources in any API
      group will be allowed.*/
     apiGroups = null,
     /**Verbs is a list of Verbs that apply to ALL the ResourceKinds and AttributeRestrictions contained
      in this rule.  VerbAll represents all kinds.*/
     verbs = null,
     /**NonResourceURLs is a set of partial urls that a user should have access to.  *s are allowed,
      but only as the full, final step in the path Since non-resource URLs are not namespaced,
      this field is only applicable for ClusterRoles referenced from a ClusterRoleBinding. Rules
      can either apply to API resources (such as "pods" or "secrets") or non-resource URL paths
      (such as "/api"),  but not both.*/
     nonResourceURLs = null,
     /**Resources is a list of resources this rule applies to.  ResourceAll represents all resources.*/
     resources = null,
     /**ResourceNames is an optional white list of names that the rule applies to.  An empty set
      means that everything is allowed.*/
     resourceNames = null
}