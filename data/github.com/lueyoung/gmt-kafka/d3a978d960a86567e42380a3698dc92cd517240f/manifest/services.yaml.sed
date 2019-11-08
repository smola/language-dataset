---
apiVersion: v1
kind: Service
metadata:
  namespace: default
  labels:
    app: kafka-1 
  name: kafka-1
spec:
  clusterIP: None 
  selector:
    statefulset.kubernetes.io/pod-name: kafka-1
  ports:
    - port: 9092
      targetPort: 9092
---
apiVersion: v1
kind: Service
metadata:
  namespace: default
  labels:
    app: kafka-2 
  name: kafka-2
spec:
  clusterIP: None 
  selector:
    statefulset.kubernetes.io/pod-name: kafka-2
  ports:
    - port: 9092
      targetPort: 9092
---
apiVersion: v1
kind: Service
metadata:
  namespace: default
  labels:
    app: kafka-0 
  name: kafka-0
spec:
  clusterIP: None 
  selector:
    statefulset.kubernetes.io/pod-name: kafka-0
  ports:
    - port: 9092
      targetPort: 9092
