#' class for creation of AHP hierarchies
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#' 
#' @examples
#' pm <- rbind(
#'   c(1, 5, 7, 5, 9, 9),
#'   c(0.2, 1, 5, 5, 7, 7),
#'   c(0.1429, 0.2, 1, 0.3333, 3, 3),
#'   c(0.2, 0.2, 3, 1, 5, 5),
#'   c(0.1111, 0.1429, 0.3333, 0.2, 1, 0.3333),
#'   c(0.1111, 0.1429, 0.3333, 0.2, 3, 1)
#' )
#' rownames(pm) <- colnames(pm) <- c("citizens",	"CI (elements)",
#'   "public infrastrucutre (trasport)",	"PI (technical infr.)",
#'   "PI (civil amenities)", "environment")
#' t <- ahpNode$new(pm = pm, leaf = FALSE, parent_w = 1)
#'
#' citizens <- rbind(
#'   c(1, 7, 8),
#'   c(0.14, 1, 3),
#'   c(0.125, 0.33, 1)
#' )
#' rownames(citizens) <- colnames(citizens) <- c("no. of cit.",
#'   "characterstic of development",	"public space")
#' t$child_nodes[[1]] <- ahpNode$new(citizens, leaf = TRUE,
#'   parent_w = t$scaled_weights[1], name = "citizens")
#'
#' pi_transport <- rbind(
#'   c(1, 0.33, 3, 7),
#'   c(3, 1, 5, 7),
#'   c(0.33, 0.2, 1, 3),
#'   c(0.14, 0.14, 0.33, 1)
#' )
#' rownames(pi_transport) <- colnames(pi_transport) <- c("rail	road",
#'   "airports", "water", "ways")
#' t$child_nodes[[3]] <- ahpNode$new(pi_transport, leaf = TRUE,
#'   parent_w = t$scaled_weights[3], name = "public infrastrucutre (trasport)")
#'
#' pi_tech <- rbind(
#'   c(1, 0.33, 0.33, 5),
#'   c(3, 1, 3, 5),
#'   c(3, 0.33, 1, 5),
#'   c(0.2, 0.2, 0.2, 1)
#' )
#' rownames(pi_tech) <- colnames(pi_tech) <- c("power line",
#'   "public water supply", "sewer watters", "gas pipeline")
#' t$child_nodes[[4]] <- ahpNode$new(pi_tech, leaf = TRUE,
#'   parent_w = t$scaled_weights[4], name = "PI (technical infr.)")
#'
#' pi_civil <- rbind(
#'   c(1, 3),
#'   c(0.33, 1)
#' )
#' rownames(pi_civil) <- colnames(pi_civil) <- c("important assets",
#'   "cultural heritage")
#' t$child_nodes[[5]] <- ahpNode$new(pi_civil, leaf = TRUE,
#'   parent_w = t$scaled_weights[5], name = "PI (civil amenities)")
#'
#' env1 <- rbind(
#'   c(1, 5),
#'   c(0.2, 1)
#' )
#' rownames(env1) <- colnames(env1) <- c("biotic", "bonity of soil")
#' t$child_nodes[[6]] <- ahpNode$new(pi_civil, leaf = TRUE, parent_w = t$scaled_weights[6], "environment")
#' t$textTree()
#' t$plotTree()
ahpNode <- R6Class(
  "ahpNode",
  public = list(
    #' @field pm preference matrix
    pm = NULL,

    #' @field leaf TRUE if the node is leaf (has no child)
    leaf = TRUE,

    #' @field parent_w weight of the parent node that is being distributed
    #'  into weights of the criteria
    parent_w = 1,

    #' @field child_nodes list of children nodes (of ahpNode type)
    child_nodes = NULL,

    #' @field scaled_weights rescaled AHP priority vector to the size of
    #'  parent_w (scaled_weights sums up to parent_w)
    scaled_weights = NULL,

    #' @field CR consistency ratio
    CR = 0,

    #' @field name name of the node in the hierarchy
    name = NULL,

    #' @description
    #' Creates node in AHP hierarchy
    #'
    #' @param pm preference matrix
    #' @param leaf TRUE if the node is leaf (has no child)
    #' @param parent_w weight of the parent node that is being distributed
    #'  into weights of the criteria
    #' @param name name of the node in the hierarchy
    initialize = function(pm, leaf = TRUE, parent_w = 1, name = "Root") {
      self$name <- name
      if (!is.null(pm)) {
        if (parent_w < 0 || parent_w > 1) {
          stop("weights must be 1 at max.")
        }
        self$pm <- pm
        t <- mcda_pairwise_weights(pm, method = "AHP")
        scaled_weights <- t$w * parent_w
        self$CR <- t$CR
      } else {
        scaled_weights <- NULL
      }
      if (!leaf && !is.null(pm)) {
        self$child_nodes <- list()
        criteria_names <- colnames(pm)
        if (is.null(criteria_names)) {
          criteria_names <- paste0("C", 1:ncol(pm))
        }
        for (i in 1:ncol(pm)) {
          self$child_nodes[[i]] <- ahpNode$new(
            pm = NULL,
            leaf = TRUE,
            parent_w = scaled_weights[i],
            name = criteria_names[i]
          )
        }
      } else if (!leaf) {
        stop("No hierarchy can be derived without preference matrix.")
      }
      self$leaf <- leaf
      self$parent_w <- parent_w
      self$scaled_weights <- scaled_weights
    },

    #' @description
    #' plots the tree from this node downwar in the hierarchy. Call from root
    #'  node of the hierarchy to plot whole tree
    plotTree = function() {
      dt <- self$to_data_tree()
      nodes <- data.frame(
        id = dt$Get("pathString"),
        label = dt$Get("name"),
        title = paste0("w: ", round(dt$Get("weight"), 3)), # Tooltip on mouse over
        shape = "box",
        color = list(background = "#f0f0f0", border = "#2b7ce9")
      )

      edges <- data.frame(
        from = dt$Get(function(x) x$parent$pathString),
        to = dt$Get("pathString")
      )

      # root does not have parent
      edges <- edges[!is.na(edges$from), ]

      visNetwork(nodes, edges) %>%
        visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    },

    #' @description
    #' prints tree in text form
    textTree = function() {
      dt <- self$to_data_tree()
      print(dt)
    },

    #' @description
    #' convert R6 structure to data.tree object for easy processing
    #'
    #' @param node_obj ahpNode object
    to_data_tree = function(node_obj = self) {
      label <- paste0(node_obj$name, "\n(w: ", round(node_obj$parent_w, 3), ")")
      if (!is.null(node_obj$pm) && ncol(node_obj$pm) > 2) {
        label <- paste0(label, "\nCR: ", round(node_obj$CR, 3))
      }

      dt_node <- Node$new(label)
      dt_node$weight <- node_obj$parent_w

      if (!is.null(node_obj$child_nodes)) {
        for (child in node_obj$child_nodes) {
          dt_node$AddChildNode(self$to_data_tree(child))
        }
      } else if (!is.null(node_obj$pm)) {
        # for leaf node prind criteria weights
        cnames <- colnames(node_obj$pm)
        w <- node_obj$scaled_weights
        for (i in 1:ncol(node_obj$pm)) {
          label <- paste0(cnames[i], "\n(w: ", round(w[i], 3), ")")
          dt_node2 <- Node$new(label)
          dt_node$AddChildNode(dt_node2)
        }
      }
      return(dt_node)
    }
  )
)