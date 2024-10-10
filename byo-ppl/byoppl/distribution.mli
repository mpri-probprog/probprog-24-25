(** Distributions and basic statistical functions. Whenever possible samplers and log-density functions use the {{: https://ocaml.xyz/owl/owl/Owl_stats/index.html}Owl_stats} implementation. *)

type 'a t
(** Distribution type. *)

val draw : 'a t -> 'a
(** Draw a sample from the distribution. *)

val logpdf : 'a t -> 'a -> float
(** [logpdf dist x] returns the value of the log-density of [dist] on [x]. *)

val get_support : 'a t -> ('a * float) list
(** Returns the support of a categorical distribution. Raise [Invalid_argument] if support is not defined. *)

val categorical_to_list : 'a t -> ('a * float) list
(** Return a list of pairs [(value, probability)]. *)

(** {1 Basic statistical functions} *)

val mean : float t -> float
(** [mean dist] returns the mean of [dist]. *)

val mean_int : int t -> float
(** Same as [mean] for distributions over integers. *)

val var : float t -> float
(** [var dist] returns the variance of [dist]. *)

val var_int : int t -> float
(** Same as [var] for distributions over integers. *)

val std : float t -> float
(** [std dist] returns the standard deviation of [dist]. *)

val std_int : int t -> float
(** Same as [std] for distributions over integers. *)

val stats : float t -> float * float
(** [stats dist] returns the mean and standard deviation of [dist]. *)

val stats_int : int t -> float * float
(** Same as [stats] for distributions over integers. *)

val split : ('a * 'b) t -> 'a t * 'b t
(** [split dist] turns a distribution over pairs into a pair of distributions. *)

val split_list : 'a list t -> 'a t list
(** [split_list dist] turns a distribution over lists into a list of distributions. *)

val split_array : 'a array t -> 'a t array
(** [split_array dist] turns a distribution over arrays into an array of distributions. *)

(** {1 Discrete distributions} *)

val bernoulli : p:float -> bool t
(** {{: https://en.wikipedia.org/wiki/Bernoulli_distribution}Bernoulli distribution} of parameter [p]. *)

val binomial : p:float -> n:int -> int t
(** {{: https://en.wikipedia.org/wiki/Binomial_distribution}Binomial distribution} of parameter [p] and [n]. *)

val dirac : v:'a -> 'a t
(** {{: https://en.wikipedia.org/wiki/Dirac_distribution}Dirac distribution} on the value [v]. *)

val categorical : support:('a * float) list -> 'a t
(** Distribution of a random variable defined by a finite support [(v0, l0), (v1, l1), ..., (vn, ln)] which can take any value [vi] with probability [exp li] (probabilities are stored in log scale for numerical stability). *)

val empirical : samples:'a list -> 'a t
(** Support distribution where all values are equiprobable. *)

val poisson : lambda:float -> int t

(** {1 Continuous distributions} *)

val beta : a:float -> b:float -> float t
(** {{: https://en.wikipedia.org/wiki/Beta_distribution}Beta distribution} with parameters [a] and [b]. *)

val gaussian : mu:float -> sigma:float -> float t
(** {{: https://en.wikipedia.org/wiki/Normal_distribution}Gaussian distribution} with mean [mu] and standard deviation [sigma]. *)

val uniform : a:float -> b:float -> float t
(** {{: https://en.wikipedia.org/wiki/Continuous_uniform_distribution}Uniform distribution} on the segment [[a, b]]. *)

(** {1 Misc utilities functions} *)

val log_sum_exp : float list -> float
(** [log_sum_exp scores] computes the logarithm of the sum of the exponentials of the arguments. *)

val normalize : ('a * float) list -> ('a * float) list
(** Turn a support expressed in logits into a finite probability distribution (where scores sum to 1).*)

val take : int -> 'a list -> 'a list
(** [take n l] only keeps the first [n] values of the list [l]. *)

val drop : int -> 'a list -> 'a list
(** [drop n l] drops the first [n] value of the list [l]. *)
